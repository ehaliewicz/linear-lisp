-- linear lisp compiler

import Data.List
import Debug.Trace
-- TODO: parser


data Expr =
  Lambda String Expr |
  App Expr Expr |
  Begin [Expr] |
  If Expr Expr Expr |
  Symbol String |
  Num Int |
  Boolean Bool |
  Nil

-- default show instances aren't pretty enough
instance Show Expr where
  show (Lambda s body) = "(\\" ++ s ++ " -> " ++
                           (show body) ++ ")"
  show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")" 
  show (Begin exps) = (foldl (++) [] (intersperse " " (map show exps)))
  show (If tst e2 e3) = "(if " ++ (foldl (++) []
                                   (intersperse " " (map show [tst, e2, e3])))
  show (Symbol s) = s
  show (Num n) = (show n)
  show (Boolean b) = (show b)
  show Nil = "nil"

type Env = [String]


-- make all binding names unique
renameVars :: Expr -> Expr
renameVars e =
  let (expr, _) = renameVars' e 0 [] in
  expr
  where
       
    
    renameVars' (Lambda arg body) i env =
      let arg' = (arg ++ (show i)) in
      let i' = (i+1) in
      let env' = ((arg, arg'):env) in
      let (body', i'') = (renameVars' body i' env') in
      ((Lambda arg' body'), i'')

    renameVars' (App e1 e2) i env =
      let (e1', i') = renameVars' e1 i env in
      let (e2', i'') = renameVars' e2 i' env in
      ((App e1' e2'), i'')

    renameVars' (Begin []) i _ =
      ((Begin []), i)

    renameVars' (Begin (e:es)) i env =
      let (e', i') = renameVars' e i env in
      let ((Begin es'), i'') = renameVars' (Begin es) i' env in
      ((Begin (e':es')), i'')
    
    --renameVars' (Be
    
    renameVars' (Symbol s) i env =
      case (lookup s env) of
        (Just b) -> ((Symbol b), i)
        Nothing -> error $ "Symbol '" ++ (show s) ++ "' is unbound."

    renameVars' _ _ _ =
      undefined
      
    

-- get free variables for an expression
-- all variables have been given unique names,
-- so we don't need a set to keep track of duplicates

freeVars :: Expr -> Env -> [Expr] 

freeVars (Lambda arg body) env =
  freeVars body (arg:env)

freeVars (App operator operand) env =
  (freeVars operator env) ++ (freeVars operand env)

freeVars (Begin exprs) env =
  (foldl (++) [] (map (\x -> (freeVars x env)) exprs))

freeVars (Symbol name) env =
  case (elemIndex name env) of
    Just _ -> []
    Nothing -> [(Symbol name)]

freeVars (If e1 e2 e3) env =
  foldl (++) [] (map (\x -> freeVars x env) [e1,e2,e3])

freeVars (Num _) _ = []
                     
freeVars (Boolean _) _ = []

freeVars (Nil) _ = []

    

-- analyzed expression
data AExpr =
  ALambda String AExpr |
  AClosure String [AExpr] AExpr |
  AApp AExpr AExpr |
  ABegin [AExpr] |
  AIf AExpr AExpr AExpr |
  ASymbol String Int |
  ANum Int |
  ABoolean Bool |
  ANil

-- same as above
instance Show AExpr where
  show (ALambda s body) = ("(\\" ++ s ++ " -> " ++
                           (show body) ++ ")")
  show (AClosure s fvs body) =
    ("(\\" ++ s ++ " {" ++
     (foldl (++) [] (intersperse "," (map show fvs))) ++
      "} " ++
      (show body) ++ ")")
  show (AApp e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")" 
  show (ABegin exps) = (foldl (++) [] (intersperse " " (map show exps)))
  show (AIf t e1 e2) =
    "(if " ++ (foldl (++) [] (intersperse " " (map show [t,e1,e2]))) ++ ")" 
  show (ASymbol s _) = s
  show (ANum i) = (show i)
  show (ABoolean b) = (show b)
  show (ANil) = "nil"

-- analyzes symbol references (determines static offset into environment)
-- differentiates closures and combinators (lambdas without free variables)
-- calculates free variables for closures
analyze :: Expr -> AExpr
analyze expr =
  analyze' expr []
  where
    analyze' (Lambda arg body) env =
      case (freeVars body [arg]) of
        [] -> (ALambda arg (analyze' body (arg:env)))
        fvs -> (AClosure arg (map (\x -> (analyze' x env)) fvs)
                (analyze' body (arg:env)))
           
    analyze' (App e1 e2) env =
      (AApp (analyze' e1 env) (analyze' e2 env))
  
    analyze' (Begin exprs) env =
      (ABegin (map (\x -> (analyze' x env)) exprs))

    analyze' (If t e1 e2) env =
      (AIf (analyze' t env)
           (analyze' e1 env)
           (analyze' e2 env))
      
    analyze' (Symbol name) env =
      case (elemIndex name env) of
        Just a -> (ASymbol name a)
        Nothing -> error $ "Symbol '" ++ name ++ "' is unbound."

    analyze' (Num i) _ =
      (ANum i)

    analyze' (Boolean b) _ =
      (ABoolean b)

    analyze' Nil _ = ANil


-- preliminary 'byte' code for a stack machine,
-- uses nested code sequences, no integer addresses
data PCode =
  CLit Int |
  CVector Int | -- number of values to pull into vector
  CClosure [PCode] | -- num-free-vars code-address 
  CLambda [PCode] | -- code address
  CDrop | -- num-drop-els
  C2Drop |
  CRoll Int | -- offset
  CApply |
  CJump String | -- jump to string label
  CLabel String |
  CBranchNot Int |
  CRet |
  CHalt |
  CBool Bool |
  CNil

-- pretty printing
instance Show PCode where
  show (CLit a) = "(Lit " ++ (show a) ++ ")"
  show (CVector i) = "(Vector " ++ (show i) ++ ")"
  show (CClosure body) =
    "(Closure " ++ (show body) ++ ")"
  show (CLambda body) =
    "(Lambda " ++ (show body) ++ ")"
  show (CDrop) = "(Drop)"
  show (C2Drop) = "(2Drop)"
  show (CRoll i) = "(Roll " ++ (show i) ++ ")"
  show (CApply) = "(Apply)" 
  show (CJump l) = "(Jump " ++ (show l) ++ ")"
  show (CBranchNot o) = "(BranchIfNot " ++ (show o) ++ ")"                 
  show (CLabel l) = "(Label " ++ (show l) ++ ")"
  show (CRet) = "(Ret)"
  show (CHalt) = "(Halt)"
  show (CBool b) = "(Bool " ++ (show b) ++ ")"
  show (CNil) = "(Nil)"


-- represents items on the stack
-- helps us keep track what would be on the runtime stack at certain
-- points, while compiling

-- this whole strategy is kind of hacky and i'd like to improve it at
-- some point but it works
data StackEl =
  -- a value, not a bound variable in this scope
  -- (could just be intermediate data, a return value, etc)
  NoStkVar |
  -- some variable bound in this scope
  StkVar String
  deriving (Show)


type CStk = [StackEl] 

foldCode _ [] stk =
  ([], stk)

foldCode f (exp:exprs) stk =
  let (code, stk') = f exp stk in
  let (recCode ,stk'') = foldCode f exprs stk' in
  (code ++ recCode, stk'')
  

symToStackVar (ASymbol s _) = (StkVar s)

symToStackVar _ = undefined

-- determines initial stack for a closure
-- we ignore anything below this on the stack
getClosureStack arg freeVars =
  -- get names of free vars and push on stack
  ((StkVar arg):(map symToStackVar freeVars))


rollStack stk 0 =
  stk

rollStack (a:b:bs) 1 =
  (b:a:bs)

rollStack (a:as) idx =
  rollStack (a:(rollStack as (idx-1))) 1

rollStack stk idx =
  error $ "Can't do " ++
  (show idx) ++ " roll on stack of length: " ++
  (show (length stk))

generateCode :: AExpr -> ([PCode], CStk)
generateCode expr =
  let (code, stk) = generate expr [] in
  (code ++ [(CHalt)], stk)


-- generate preliminary stack machine code
-- doesn't have integer addresses!
generate :: AExpr -> CStk -> ([PCode], CStk)
generate (ALambda name body) stk =
  let (bodyCode, lstk) = (generate body [(StkVar name)]) in
  if (length lstk) == 1
  then ([(CLambda (bodyCode ++ [(CRet)]))], ((NoStkVar):stk))
  --trace ("Stack after calling lambda " ++ (show (ALambda name body)) ++
  --       ": " ++ (show lstk))
  else error "Variable passed to lambda not consumed!"

  
generate (AClosure name fvs body) stk =
  let beforeClosureStack = (getClosureStack name fvs) in
  let (fvsCode, fvsStk) = (foldCode generate fvs stk) in
  let (bodyCode, clsStk) = (generate body beforeClosureStack) in
  if (length clsStk) == 1
  then (fvsCode ++ [(CVector (length fvs)),
                    (CClosure (bodyCode ++ [(CRet)]))],

        ((NoStkVar):(drop (length fvs) fvsStk)))
  --trace ("Stack at beginning of closure " ++ (show (AClosure name fvs body)) ++
  --       ": " ++ (show beforeClosureStack) ++ 
  --       "\nStack after calling closure " ++ (show (AClosure name fvs body)) ++
  --       ": " ++ (show clsStk))
  else error "Variable passed to lambda not consumed!"
           

generate (AApp e1 e2) stk =
  let (ocode, nstk) = (generate e2 stk) in
  let (fcode, nstk2) = (generate e1 nstk) in
  
  ((ocode++fcode++[(CApply)]) , (NoStkVar):(drop 2 nstk2))

generate (ABegin []) stk =
  ([], stk)

generate (ABegin [expr]) stk =
  (generate expr stk)

generate (ABegin (e:es)) stk =
  let (ecode, estk) = (generate e stk) in
  let (rcode, rstk) = (generate (ABegin es) (drop 1 estk)) in
  ((ecode ++ [(CDrop)] ++ rcode), rstk)


generate (ASymbol name _) stk =
  case (findIndex (\ el ->
                    case el of
                      NoStkVar -> False
                      (StkVar sname) -> name == sname) stk) of
    Nothing -> error $ "Can't find symbol '" ++ (show name) ++ "' on stack!"
    Just idx ->
      if idx == 0
      then ([], stk)
      else ([(CRoll idx)], (rollStack stk idx))

generate (ANum i) stk =
  ([(CLit i)], (NoStkVar:stk))

generate (ABoolean b) stk =
  ([(CBool b)], (NoStkVar:stk))

generate (ANil) stk =
  ([(CNil)], (NoStkVar:stk))

generate (AIf test e1 e2) stk =
  let (tcode, tstk) = (generate test stk) in
  let (e1code, e1stk) = (generate e1 (drop 1 stk)) in
  let (e2code, e2stk) = (generate e2 (drop 1 tstk)) in
  if (length e1stk) /= (length e2stk)
  then error "If-arm variable usage not balanced!"
  else ((tcode ++
         ((CBranchNot (length e1code)): e1code ++ e2code)),
        e2stk)
        
  

peep ((CRoll 1):(CRoll 1):rst) = (peep rst)

peep ((CDrop):(CDrop):rst) = ((C2Drop):(peep rst))

peep ((CClosure body):rst) =
  ((CClosure (peep body)):(peep rst))

peep ((CLambda body):rst) =
  ((CLambda (peep body)):(peep rst))

peep (a:as) = (a:(peep as))

peep [] = []


type LabelMap = [(String, Int)]

type DelayedJumpFunc = (LabelMap -> Int)

-- expanded code, no labels
data ECode =
  ELit Int |
  EVector Int | -- num values to pull into vector
  EClosure Int | -- code-address 
  ELambda Int | -- code address
  EDrop | -- num-drop-els
  E2Drop |
  ERoll Int | -- offset
  EApply |
  EDelayedJump DelayedJumpFunc |
  EJump Int | -- jump to relative integer address
  EBranchNot Int | -- jump to relative integer address
  ERet |
  EHalt |
  EBool Bool |
  ENil


instance Show ECode where
  show (ELit a) = "(Lit " ++ (show a) ++ ")"
  show (EVector i) = "(Vector " ++ (show i) ++ ")"
  show (EClosure bodyAddr) =
    "(Closure " ++ (show bodyAddr) ++ ")"
  show (ELambda bodyAddr) =
    "(Lambda " ++ (show bodyAddr) ++ ")"
  show (EDrop) = "(Drop)"
  show (E2Drop) = "(2Drop)"
  show (ERoll i) = "(Roll " ++ (show i) ++ ")"
  show (EApply) = "(Apply)" 
  show (EJump i) = "(Jump " ++ (show i) ++ ")"
  show (EBranchNot i) = "(BranchIfNot " ++ (show i) ++ ")"
  show (EDelayedJump _) = "(DelayedJump <f>)"
  show (ERet) = "(Ret)"
  show (EHalt) = "(Halt)"
  show (EBool b) = "(Bool " ++ (show b) ++ ")"
  show (ENil) = "(Nil)"



-- translate preliminary code (no address references) to expanded code
  -- (with addresses)


translate :: [PCode] -> [ECode]

translate code = 
  let (translatedCode, _, _) = translate' code [] 0 in
  translatedCode
  where
    --translateSimple :: (Maybe ECode) -> [PCode] -> LabelMap -> Int -> ([ECode], LabelMap, Int)
    translateSimple transCode restCode map off =
      case transCode of
        Just code -> 
          let (recurCode, recurMap, recurOff) = translate' restCode map (off+1) in
          ((code:recurCode), recurMap, recurOff)
        Nothing -> 
          let (recurCode, recurMap, recurOff) = translate' restCode map off in
          (recurCode, recurMap, recurOff)
    
    translate' ((CLit i):rs) map off =
      translateSimple (Just (ELit i)) rs map off

    translate' ((CVector i):rs) map off =
      translateSimple (Just (EVector i)) rs map off
    
    translate' ((CClosure body):rs) map off =
      let (recurCode, recurMap, recurOff) = translate' rs map (off+1) in
      let (closCode, closMap, closOff) = translate' body recurMap recurOff in
      (((EClosure recurOff):recurCode)++closCode, closMap, closOff)

    translate' ((CLambda body):rs) map off =
      let (recurCode, recurMap, recurOff) = translate' rs map (off+1) in
      let (lamCode, lamMap, lamOff) = translate' body recurMap recurOff in
      (((ELambda recurOff):recurCode)++lamCode, lamMap, lamOff)

    translate' ((CDrop):rs) map off =
      translateSimple (Just (EDrop)) rs map off

    translate' ((C2Drop):rs) map off =
      translateSimple (Just (E2Drop)) rs map off
      
    translate' ((CRoll i):rs) map off =
      translateSimple (Just (ERoll i)) rs map off

    translate' ((CApply):rs) map off =
      translateSimple (Just (EApply)) rs map off

    translate' ((CJump l):rs) map off =
      translateSimple
      (Just (EDelayedJump
             (\map ->
               case (find (\(name,_) -> name == l) map) of
                 Just (_, int) -> int
                 Nothing -> error $ "Label " ++ (show l) ++ " used but not defined!"
             )))
      rs map off
    
    translate' ((CLabel l):rs) map off =
      case (find (\(name, _) -> name == l) map) of 
        Just _ -> error $ "Label " ++ (show l) ++ " defined multiple times!" 
        Nothing -> translateSimple Nothing rs ((l, off):map) off

    translate' ((CRet):rs) map off =
      translateSimple (Just (ERet)) rs map off

    translate' ((CHalt):rs) map off =
      translateSimple (Just (EHalt)) rs map off

    translate' ((CBool b):rs) map off =
      translateSimple (Just (EBool b)) rs map off

    translate' ((CNil):rs) map off =
      translateSimple (Just (ENil)) rs map off

    translate' ((CBranchNot o):rs) map off =
      translateSimple (Just (EBranchNot o)) rs map off

    translate' [] map off = ([], map, off)

-- runtime objects
data RObject =
  RVector [RObject] |
  RNum Int |
  RBool Bool |
  RClosure [RObject] Int |
  RLambda Int |
  RNil



-- pretty printing
instance Show RObject where
  show (RVector objs) = "(Vector " ++ (foldl (++) [] (map show objs)) ++ ")"
  show (RNum i) = (show i)
  show (RClosure _ addr) = "<Closure addr:" ++ (show addr) ++ ">"
  show (RLambda addr) = "<Lambda addr:" ++ (show addr) ++ ">"
  show (RBool bool) = show bool
  show (RNil) = "nil"


execute :: [ECode] -> [RObject]

execute code =
  --let codeLen = (length code) in
  -- we use a data stack and a return stack
  recur 0 [] []
  where
    codeLen = (length code)
    recur pc stk rstk =
      trace (foldl (++) [] (intersperse ", " [(show pc), (show stk), (show rstk)]))
      (if pc >= codeLen
       then stk
       else (inner (code !! pc) pc stk rstk))

    inner (ELit i) pc stk rstk =
      (recur (pc+1) ((RNum i):stk) rstk)
    inner (EVector i) pc stk rstk =
      (recur (pc+1) ((RVector (take i stk)):stk) rstk)
    inner (EClosure addr) pc ((RVector objs):rs) rstk =
      (recur (pc+1) ((RClosure objs addr):rs) rstk)
    inner (EClosure _) _ _ _ =
      error "Trying to create a closure without a vector on top of the stack! Compiler error?"
    inner (ELambda addr) pc stk rstk =
      (recur (pc+1) ((RLambda addr):stk) rstk)
    inner (EDrop) pc (_:bs) rstk =
      (recur (pc+1) bs rstk)
    inner (EDrop) _ _ _ =
      error "Trying to drop an empty stack! Compiler error?"
    inner (E2Drop) pc (_:_:cs) rstk =
      (recur (pc+1) cs rstk)
    inner (E2Drop) _ _ _ =
      error "Trying to drop two items with less than two items on the stack! Compiler error?"
    inner (ERoll i) pc stk rstk =
      (recur (pc+1) (rollStack stk i) rstk)
    inner (EApply) pc (operand:operator:rest) rstk =
      let rstk' = ((pc+1):rstk) in
      case operator of
        (RClosure freeVars addr) ->
          (recur addr (operand:(freeVars++rest)) rstk')
        (RLambda addr) ->
          (recur addr (operand:rest) rstk')
        _ -> error "Trying to apply something that isn't a Lambda or Closure! Compiler error?"
    inner (EApply) _ _ _ =
      error "Apply without enough items on the stack! Compiler error?"
    inner (EJump offset) pc stk rstk =
      (recur (pc+1+offset) stk rstk)
    inner (EBranchNot offset) pc (val:stk) rstk =
      case val of
        (RNil) -> (recur (pc+1) stk rstk)
        (RBool False) -> (recur (pc+1) stk rstk)
        _ -> (recur (pc+1+offset) stk rstk)
    inner (EBranchNot _) _ _ _ =
      error "Can't execute BranchIfNot with no items on stack!"
    inner (ERet) _ stk (retAddr:rstk) =
      (recur retAddr stk rstk)
    inner (ERet) _ _ _ =
      error "Return instruction without a return address! Compiler error?"
    inner (EDelayedJump _) _ _ _ =
      error "Cannot execute a non-resolved jump instruction! Compiler error?"
    inner (EHalt) _ stk _ =
      stk
    inner (EBool b) pc stk rstk =
      (recur (pc+1) ((RBool b):stk) rstk)

    inner (ENil) pc stk rstk =
      (recur (pc+1) ((RNil):stk) rstk)
  
    
main = 
  let expr =
        (App (App (Lambda "x" (Lambda "y" (Begin [(Symbol "y"), (Symbol "x")])))
                   (Lambda "xbound" (Symbol "xbound")))
                  (Lambda "ybound" (Symbol "ybound"))) in
        --(Lambda "x" (Lambda "y" (Begin [(Symbol "y"), (Symbol "x"), (Num 5)])))
  let renamedExpr = (renameVars expr) in
  let analyzedExpr = (analyze renamedExpr) in
  let (generatedCode, stk) = (generateCode analyzedExpr) in
  let peepHoleCode = (peep generatedCode) in
  let expandedCode = (translate peepHoleCode) in do
  --let testStk = ["a","b","c","d"] in do
    putStrLn $ "Expr: " ++ (show expr)
    putStrLn ""
    putStrLn $ "Analyzed expr: " ++ (show analyzedExpr)
    putStrLn ""
    putStrLn $ "Generated code: " ++ (show generatedCode)
    putStrLn ""
    putStrLn $ "Stack after executing: " ++ (show stk)
    putStrLn $ "Peephole optimized code: " ++ (show peepHoleCode)
    putStrLn ""
    putStrLn $ "Expanded code: " ++ (show expandedCode)

    putStrLn $ "Result: " ++ (show (execute expandedCode))
