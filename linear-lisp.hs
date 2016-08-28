-- linear lisp compiler

import Data.List
-- TODO: parser


data Expr =
  Lambda String Expr |
  App Expr Expr |
  Begin [Expr] |
  Symbol String |
  Num Int


instance Show Expr where
  show (Lambda s body) = "(\\" ++ s ++ " -> " ++
                           (show body) ++ ")"
  show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")" 
  show (Begin exps) = (foldl (++) [] (intersperse " " (map show exps)))
  show (Symbol s) = s
  show (Num n) = (show n)

freeVars (Lambda arg body) env =
  freeVars body (arg:env)

freeVars (App operator operand) env =
  (freeVars operator env) ++ (freeVars operand env)

freeVars (Begin exprs) env =
  (foldl (++) [] (map (\x -> (freeVars x env)) exprs))

freeVars (Symbol name) env =
  case (elemIndex name env) of
    Just a -> []
    Nothing -> [(Symbol name)]

freeVars (Num i) env = []

type Env = [String]


-- analyzed expression
data AExpr =
  ALambda String AExpr |
  AClosure String [AExpr] AExpr |
  AApp AExpr AExpr |
  ABegin [AExpr] |
  ASymbol String Int |
  ANum Int

           
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
  show (ASymbol s off) = s
  show (ANum i) = (show i)

analyze :: Expr -> Env -> AExpr
analyze (Lambda arg body) env =
  case (freeVars body [arg]) of
      [] -> (ALambda arg (analyze body (arg:env)))
      fvs -> (AClosure arg (map (\x -> (analyze x env)) fvs)
              (analyze body (arg:env)))
           
analyze (App e1 e2) env =
  (AApp (analyze e1 env) (analyze e2 env))
  
analyze (Begin exprs) env =
  (ABegin (map (\x -> (analyze x env)) exprs))
  
analyze (Symbol name) env =
  case (elemIndex name env) of
    Just a -> (ASymbol name a)
    Nothing -> error $ "Symbol '" ++ name ++ "' is unbound."

analyze (Num i) env =
  (ANum i)

-- preliminary code, no integer addresses
data PCode =
  CLit Int |
  CClosure Int [PCode] | -- num-free-vars code-address 
  CLambda [PCode] | -- code address
  CDrop | -- num-drop-els
  C2Drop |
  CRoll Int | -- offset
  CApply |
  CJump String | -- jump to string label
  CLabel String |
  CRet

-- represents items on the stack
-- helps us keep track what would be on the runtime stack at certain
-- points, while compiling


data StackEl =
  -- a value, not a bound variable in this scope
  -- (could just be intermediate data, a return value, etc)
  NoStkVar |
  -- some variable bound in this scope
  StkVar String

type CStk = [StackEl] 

instance Show PCode where
  show (CLit a) = "(Lit " ++ (show a) ++ ")"
  show (CClosure numArgs body) =
    "(Closure " ++ (show numArgs) ++ " " ++ (show body) ++ ")"
  show (CLambda body) =
    "(Lambda " ++ (show body) ++ ")"
  show (CDrop) = "(Drop)"
  show (C2Drop) = "(2Drop)"
  show (CRoll i) = "(Roll " ++ (show i) ++ ")"
  show (CApply) = "(Apply)" 
  show (CJump l) = "(Jump " ++ (show l) ++ ")"
  show (CLabel l) = "(Label " ++ (show l) ++ ")"
  show (CRet) = "(Ret)"


generateFreeVarsCode [] stk =
  ([], stk)

generateFreeVarsCode (v:vs) stk =
  let (code, nstk) = (generate v stk) in
  let (rcode, rstk) = (generateFreeVarsCode vs nstk) in
  (code ++ rcode, rstk)

getFreeVarsStack [] = []

getFreeVarsStack ((ASymbol s off):vs) =
  ((True, s): (getFreeVarsStack vs))

getClosureStack (AClosure arg freeVars body) =
  ((True, arg):(getFreeVarsStack freeVars))


rollStack stk 0 =
  stk

rollStack (a:b:bs) 1 =
  (b:a:bs)

rollStack (a:as) idx =
  rollStack (a:(rollStack as (idx-1))) 1


-- generate preliminary stack machine code
-- doesn't have integer addresses

generate :: AExpr -> CStk -> ([PCode], CStk)
generate (ALambda name body) stk =
  let (bodyCode, lstk) = (generate body [(StkVar name)]) in
  if (length lstk) == 1
  then ([(CLambda (bodyCode ++ [(CRet)]))], ((NoStkVar):stk))
  --trace ("Stack after calling lambda " ++ (show (ALambda name body)) ++
  --       ": " ++ (show lstk))
  else error "Variable passed to lambda not consumed!"

  
generate (AClosure name fvs body) stk =
  let beforeClosureStack = (getClosureStack (AClosure name fvs body)) in
  let (fvsCode, fvsStk) = (generateFreeVarsCode fvs stk) in
  let (bodyCode, clsStk) = (generate body beforeClosureStack) in
  if (length clsStk) == 1
  then ([(CClosure (length fvs) (bodyCode ++ [(CRet)]))],
        ((False, ""):(drop (length fvs) fvsStk)))
  --trace ("Stack at beginning of closure " ++ (show (AClosure name fvs body)) ++
  --       ": " ++ (show beforeClosureStack) ++ 
  --       "\nStack after calling closure " ++ (show (AClosure name fvs body)) ++
  --       ": " ++ (show clsStk))
  else error "Variable passed to lambda not consumed!"
           

generate (AApp e1 e2) stk =
  let (ocode, nstk) = (generate e2 stk) in
  let (fcode, nstk2) = (generate e1 nstk) in
  
  ((ocode++fcode++[(CApply)]) , (False, ""):(drop 2 nstk))

generate (ABegin []) stk =
  ([], stk)

generate (ABegin [expr]) stk =
  (generate expr stk)

generate (ABegin (e:es)) stk =
  let (ecode, estk) = (generate e stk) in
  let (rcode, rstk) = (generate (ABegin es) (drop 1 estk)) in
  ((ecode ++ [(CDrop)] ++ rcode), rstk)


generate (ASymbol name off) stk =
  case (findIndex (\(isVar,sName) -> name == sName) stk) of
    Nothing -> error $ "Can't find symbol '" ++ (show name) ++ "' on stack!"
    Just idx ->
      if idx == 0
      then ([], stk)
      else ([(CRoll idx)], (rollStack stk idx))

generate (ANum i) stk =
  ([(CLit i)], ((False, ""):stk))


peep ((CRoll 1):(CRoll 1):rst) = (peep rst)

peep ((CDrop):(CDrop):rst) = ((C2Drop):(peep rst))

peep ((CClosure num body):rst) =
  ((CClosure num (peep body)):(peep rst))

peep ((CLambda body):rst) =
  ((CLambda (peep body)):(peep rst))

peep (a:as) = (a:(peep as))

peep [] = []


type LabelMap = [(String, Int)]

type DelayedJumpFunc = (LabelMap -> Int)

-- expanded code, no labels
data ECode =
  ELit Int |
  EClosure Int Int | -- num-free-vars code-address 
  ELambda Int | -- code address
  EDrop | -- num-drop-els
  E2Drop |
  ERoll Int | -- offset
  EApply |
  EDelayedJump DelayedJumpFunc |
  EJump Int | -- jump to integer address
  ERet


instance Show ECode where
  show (ELit a) = "(Lit " ++ (show a) ++ ")"
  show (EClosure numArgs bodyAddr) =
    "(Closure " ++ (show numArgs) ++ " " ++ (show bodyAddr) ++ ")"
  show (ELambda bodyAddr) =
    "(Lambda " ++ (show bodyAddr) ++ ")"
  show (EDrop) = "(Drop)"
  show (E2Drop) = "(2Drop)"
  show (ERoll i) = "(Roll " ++ (show i) ++ ")"
  show (EApply) = "(Apply)" 
  show (EJump i) = "(Jump " ++ (show i) ++ ")"
  show (EDelayedJump _) = "(DelayedJump <f>)"
  show (ERet) = "(Ret)"





-- translate preliminary code (no address references) to expanded code
  -- (with addresses)


translate :: [PCode] -> [ECode]

translate code = 
  let (translatedCode, map, off) = translate' code [] 0 in
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
    
    translate' ((CClosure i body):rs) map off =
      let (recurCode, recurMap, recurOff) = translate' rs map (off+1) in
      let (closCode, closMap, closOff) = translate' body recurMap recurOff in
      (((EClosure i recurOff):recurCode)++closCode, closMap, closOff)

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
               case (find (\(name,int) -> name == l) map) of
                 Just (name, int) -> int
                 Nothing -> error $ "Label " ++ (show l) ++ " used but not defined!"
             )))
      rs map off
    
    translate' ((CLabel l):rs) map off =
      case (find (\(name, int) -> name == l) map) of 
        Just a -> error $ "Label " ++ (show l) ++ " defined multiple times!" 
        Nothing -> translateSimple Nothing rs ((l, off):map) off

    translate' ((CRet):rs) map off =
      translateSimple (Just (ERet)) rs map off

    translate' [] map off = ([], map, off)
  
main = 
  let expr =
        -- (App (App (Lambda "x" (Lambda "y" (Begin [(Symbol "y"), (Symbol "x")])))
        --            (Lambda "xbound" (Symbol "xbound")))
        --           (Lambda "ybound" (Symbol "ybound")))
        (Lambda "x" (Lambda "y" (Begin [(Symbol "y"), (Symbol "x"), (Num 5)])))
  in
  let analyzedExpr = (analyze expr []) in
  let (generatedCode, stk) = (generate analyzedExpr []) in
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
    putStrLn $ "Optimized code: " ++ (show peepHoleCode)
    putStrLn ""
    putStrLn $ "Expanded code: " ++ (show expandedCode)
