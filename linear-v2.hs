import Data.List (lookup, elemIndex)

import Data.IORef (readIORef, modifyIORef, newIORef, IORef)

import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

import Text.ParserCombinators.Parsec


type Name = String

data Type =
  TInt |
  TBool |
  TParam Int |
  TTimes Type Type |
  TArrow Type Type |
  TList Type
  deriving (Eq)

typeNames = (map (:[]) ['a'..'z'])

instance Show Type where 
  show (TInt) = "Int"
  show (TBool) = "Bool"
  show (TParam k) =
    if k < length(typeNames)
    then typeNames !! k
    else "'ty" ++ (show k)

  show (TList t) =
    "[" ++ (show t) ++ "]"

  show (TTimes t1 t2) =
    "(" ++ (show t1) ++ ", " ++ (show t2) ++ ")"

  show (TArrow t1 t2) =
    "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
    

data Expr =
  Var Name |
  Num Int |
  Boolean Bool |
  Times Expr Expr |
  Divide Expr Expr |
  Mod Expr Expr |
  Plus Expr Expr |
  Minus Expr Expr |
  Equal Expr Expr |
  Less Expr Expr |
  If Expr Expr Expr |
  Fun Name Expr |
  Apply Expr Expr |
  Pair Expr Expr |
  Fst Expr |
  Snd Expr |
  Rec Name Expr |
  Nil |
  Cons Expr Expr |
  Dup Expr |
  Match Expr Expr Name Name Expr  
  deriving (Show, Eq)

data ToplevelCmd =
  Expr Expr |
  Def Name Expr |
  Use String |
  Quit

-- rename type parameters to count up from 0
-- makes it easier to keep track of type vars that might have had the
-- same name
rename typ =
  fst (ren (0,[]) typ)
  where


    -- c is (Int, [(Int, Int)])

    ren c TInt = (TInt, c)

    ren c TBool = (TBool, c)

    ren (j,s) (TParam k) =
      let c = (j, s) in
      case (lookup k s) of
        (Just k') -> ((TParam k'), c)
        Nothing -> ((TParam j), (j+1, (k,j):s))
    
    
    ren c (TArrow t1 t2) =
      let (u1, c') = ren c t1 in
      let (u2, c'') = ren c' t2 in
      ((TArrow u1 u2), c'')

    ren c (TTimes t1 t2) =
      let (u1, c') = ren c t1 in
      let (u2, c'') = ren c' t2 in
      ((TTimes u1 u2), c'')

    ren c (TList t) =
      let (u, c') = ren c t in
      ((TList u), c')

rename2 t1 t2 =
  case (rename (TTimes t1 t2)) of
    (TTimes u1 u2) -> (u1, u2)
    _ -> error "Type rename error"


-- replace free variable occurrences
subst2 s t1 t2 const =
  (const (subst s t1) (subst s t2))

subst s (Var x) =
  case (lookup x s) of
    (Just x') -> x'
    Nothing -> (Var x)

subst s (Times e1 e2) =
  subst2 s e1 e2 Times

subst s (Divide e1 e2) =
  subst2 s e1 e2 Divide
  
subst s (Mod e1 e2) =
  subst2 s e1 e2 Mod

subst s (Plus e1 e2) =
  subst2 s e1 e2 Plus

subst s (Minus e1 e2) =
  subst2 s e1 e2 Minus

subst s (Equal e1 e2) =
  subst2 s e1 e2 Equal

subst s (Cons e1 e2) =
  subst2 s e1 e2 Cons

subst s (Less e1 e2) =
  subst2 s e1 e2 Less

subst s (If e1 e2 e3) =
  (If (subst s e1) (subst s e2) (subst s e3))

subst s (Fun x e) =
  let s' = remLookup x s in
  (Fun x (subst s' e))

subst s (Rec x e) =
  let s' = remLookup x s in
  (Rec x (subst s' e))

subst s (Match e1 e2 x y e3) =
  let s' = remLookup y $ remLookup x s in
  (Match (subst s e1) (subst s e2) x y (subst s' e3))

subst s (Apply e1 e2) =
  subst2 s e1 e2 Apply

subst s (Pair e1 e2) =
  subst2 s e1 e2 Cons

subst s (Fst e) =
  (Fst (subst s e))

subst s (Snd e) =
  (Snd (subst s e))

subst _ a =
  a


remLookup key list = filter ((/=key) . fst) list 


-- replace type parameters
tsubst s (TParam k) =
  case (lookup k s) of
    (Just a) -> a
    Nothing -> (TParam k)

tsubst s (TTimes t1 t2) =
  (TTimes (tsubst s t1) (tsubst s t2))

tsubst s (TArrow t1 t2) =
  (TArrow (tsubst s t1) (tsubst s t2))

tsubst s (TList t) =
  (TList (tsubst s t))

tsubst _ a =
  a


typeError msg =
  error $ "Type error: " ++ msg

freshCounter :: IORef Int
freshCounter = unsafePerformIO (newIORef 0)



fresh = do
  var <- readIORef freshCounter
  modifyIORef freshCounter (+1)
  return (TParam var)
             

refresh typ =
  fst (refresh [] typ)
  where
    
    refresh s (TParam k) =
      case (lookup k s) of
        (Just k') -> (k', s)
        Nothing ->
          let t = unsafePerformIO fresh in
          (t, (k,t):s)
          
    refresh s (TArrow t1 t2) =
      let (u1, s') = refresh s t1 in
      let (u2, s'') = refresh s' t2 in
      ((TArrow u1 u2), s'')

    refresh s (TTimes t1 t2) =
      let (u1, s') = refresh s t1 in
      let (u2, s'') = refresh s' t2 in
      ((TTimes u1 u2), s'')

    refresh s (TList t) =
      let (u, s') = refresh s t in
      ((TList u), s')

    refresh s TInt = (TInt, s)
    refresh s TBool = (TBool, s)
    


occurs _ TInt = False
occurs _ TBool = False

occurs k (TParam j) = k == j

occurs k (TArrow t1 t2) = (occurs k t1) || (occurs k t2)

occurs k (TTimes t1 t2) = (occurs k t1) || (occurs k t2)

occurs k (TList t) = (occurs k t)



solve e =
  solve' e []
  
  where

    typErr t1 t2 = 
      let (u1, u2) = rename2 t1 t2 in
      typeError ("The types " ++ (show u1) ++ " and " ++
                 (show u2) ++ " are incompatible.")
    
    solve' ((t, (TParam k)):eq) sbst =
      if (not (occurs k t))
      then solve' (((TParam k), t):eq) sbst
      else typErr t (TParam k)
           
    solve' (((TParam k), t):eq) sbst =
      if (not (occurs k t))
       then
         let ts = tsubst [(k,t)] in
         (solve'
          (map (\(ty1,ty2) -> ((ts ty1), (ts ty2))) eq)
          ((k,t):(map (\(n,u) -> (n, (ts u))) sbst)))
       else typErr (TParam k) t

    
    solve' (((TTimes u1 v1), (TTimes u2 v2)):eq) sbst =
      (solve' ((u1, u2):(v1,v2):eq) sbst)
      
    solve' (((TArrow u1 v1), (TArrow u2 v2)):eq) sbst =
      (solve' ((u1, u2):(v1,v2):eq) sbst)
    
    solve' (((TList t1), (TList t2)):eq) sbst =
      solve' ((t1,t2):eq) sbst

    solve' ((t1, t2):eq) sbst =
      if t1 == t2
      then solve' eq sbst
      else typErr t1 t2
    
    solve' [] sbst =
      sbst


      
      

      

constraintsOf expr gctx =
  cnstr expr []
  where

    binOp typ e1 e2 ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      (typ, ((ty1,typ):(ty2,typ):eq1++eq2))
    
    cnstr (Var x) ctx =
      case (lookup x ctx) of
        (Just a) -> (a, [])
        Nothing ->
          case (lookup x gctx) of
            (Just b) -> (refresh b, [])
            Nothing -> typeError $ "Unknown variable " ++ x

    cnstr (Num _) _ = (TInt, [])

    cnstr (Boolean _) _ = (TBool, [])

    cnstr Nil _ = ((TList $ unsafePerformIO fresh), []) 

    cnstr (Times e1 e2) ctx =
      binOp TInt e1 e2 ctx

    cnstr (Divide e1 e2) ctx =
      binOp TInt e1 e2 ctx

    cnstr (Mod e1 e2) ctx =
      binOp TInt e1 e2 ctx

    cnstr (Plus e1 e2) ctx =
      binOp TInt e1 e2 ctx

    cnstr (Minus e1 e2) ctx =
      binOp TInt e1 e2 ctx

    cnstr (Less e1 e2) ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      let ty = TList ty1 in
      (TBool, ((ty1, TInt):(ty2, TInt):(eq1++eq2)))

    
    cnstr (Equal e1 e2) ctx =
      let (ty1,eq1) = cnstr e1 ctx in
      let (ty2,eq2) = cnstr e2 ctx in
      (TBool, ((ty1, ty2):(ty2, ty1):(eq1++eq2)))
      
    cnstr (Cons e1 e2) ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      let ty = (TList ty1) in
      (ty, ((ty2, ty): (eq1 ++ eq2)))

    cnstr (If e1 e2 e3) ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      let (ty3, eq3) = cnstr e3 ctx in
      (ty2, ((ty1, TBool) : (ty2, ty3) : eq1 ++ eq2 ++ eq3))

    cnstr (Fun x e) ctx =
      let ty1 = unsafePerformIO fresh in
      let (ty2, eq) = cnstr e ((x,ty1):ctx) in
      ((TArrow ty1 ty2), eq)
    
    cnstr (Rec x e) ctx =
      let ty1 = unsafePerformIO fresh in
      let (ty2, eq) = cnstr e ((x,ty1):ctx) in
      (ty1, ((ty1,ty2):eq))

    cnstr (Match e1 e2 x y e3) ctx =
      let ty = unsafePerformIO fresh in
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      let (ty3, eq3) = cnstr e3 ((x,ty):(y, (TList ty)):ctx) in
      (ty2, ((ty1, (TList ty)):(ty2, ty3):eq1++eq2++eq3))

    cnstr (Apply e1 e2) ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      let ty = unsafePerformIO fresh in
      (ty, (ty1, (TArrow ty2 ty)):eq1++eq2)

    cnstr (Pair e1 e2) ctx =
      let (ty1, eq1) = cnstr e1 ctx in
      let (ty2, eq2) = cnstr e2 ctx in
      ((TTimes ty1 ty2), (eq1++eq2))

    cnstr (Fst e) ctx =
      let (ty, eq) = cnstr e ctx in
      let ty1 = unsafePerformIO fresh in
      let ty2 = unsafePerformIO fresh in
      (ty1, ((ty, (TTimes ty1 ty2)):eq))

    cnstr (Snd e) ctx =
      let (ty, eq) = cnstr e ctx in
      let ty1 = unsafePerformIO fresh in
      let ty2 = unsafePerformIO fresh in
      (ty1, ((ty, (TTimes ty1 ty2)):eq))

typeOf e ctx =
  let (ty, eq) = constraintsOf e ctx in
  let solvedEq = (solve eq) in
  solvedEq `seq`
  tsubst (solve eq) ty

-- basic interpreter


type Environment = [(Name, Value)]

data Value =
  VInt Int |
  VBool Bool |
  VNil |
  VPair Value Value |
  VClosure Environment Expr
  deriving (Show, Eq)

runtimeError msg =
  error $ "Runtime error: " ++ msg

intBinOp e1 e2 env func retType =
  case ((interpret e1 env), (interpret e2 env)) of
    ((VInt k1), (VInt k2)) -> (retType $ k1 `func` k2) 
    _ -> error "Runtime error: Expected integer types."

interpret :: Expr -> Environment -> Value

interpret (Var x) env =
  case (lookup x env) of
    (Just a) -> a
    Nothing -> runtimeError $ "Unknown variable '" ++ x ++ "'"
    
interpret (Num k) env = (VInt k)

interpret (Boolean b) env = (VBool b)

interpret (Times e1 e2) env =
  intBinOp e1 e2 env (*) VInt

interpret (Divide e1 e2) env =
  intBinOp e1 e2 env (div) VInt

interpret (Plus e1 e2) env =
  intBinOp e1 e2 env (+) VInt

interpret (Minus e1 e2) env =
  intBinOp e1 e2 env (-) VInt

interpret (Mod e1 e2) env =
  intBinOp e1 e2 env mod VInt

interpret (Equal e1 e2) env =
  (VBool $ (interpret e1 env) == (interpret e2 env))

interpret (Less e1 e2) env =
  intBinOp e1 e2 env (<) VBool


interpret (If e1 e2 e3) env =
  case (interpret e1 env) of
    (VBool True) -> (interpret e2 env)
    (VBool False) -> (interpret e3 env)
    _ -> runtimeError "Expected boolean type."

interpret (Fun n e) env =
  (VClosure env (Fun n e))


interpret (Apply f o) env =
  let o' = interpret o env in
  let f' = interpret f env in
  case f' of
    (VClosure env (Fun arg expr)) -> (interpret expr ((arg,o'):env))
    _ -> runtimeError "Expected function."


interpret (Pair e1 e2) env =
  (VPair (interpret e1 env) (interpret e2 env))

interpret (Fst e1) env =
  case (interpret e1 env) of
    (VPair ca cd) -> ca
    _ -> runtimeError "Expected pair."

interpret (Snd e1) env =
  case (interpret e1 env) of
    (VPair ca cd) -> cd
    _ -> runtimeError "Expected pair."

interpret (Rec n e) env =
  (interpret e ((n, (VClosure env e)):env))

interpret Nil env =
  VNil

interpret (Cons e1 e2) env =
  (VPair (interpret e1 env) (interpret e2 env))

interpret (Match e1 e2 x y e3) env =
  case (interpret e1 env) of
    VNil -> interpret e2 env
    (VPair d1 d2) -> interpret e3 ((x,d1):(y,d2):env)
    _ -> runtimeError "List expected in match."

rollStack stk 0 =
  stk

rollStack (a:b:bs) 1 =
  (b:a:bs)

rollStack stk idx =
  error $ "Can't do " ++
  (show idx) ++ " roll on stack of length: " ++
  (show (length stk))

dupStack (a:as) = (a:a:as)
dupStack [] = error $ "Can't do dup on empty stack"

pickStack stk idx =
  ((stk !! idx):stk)


stackCheck Expr -> CEnvironment -> Boolean



-- type CEnvironment = [Name]
-- type MEnvironment = [Value]

-- data Inst =
--   IAdd |
--   ISub |
--   IMult |
--   IDiv |
--   IEqual |
--   ILess |
--   IVar Int |
--   IInt Int |
--   IBool Bool |
--   IClosure [Inst] MEnvironment |
--   IIf [Inst] [Inst] |
--   ICall |
--   IPopEnv
--   deriving (Show, Eq)

-- compileError msg =
--   error $ "Compile-time error: " ++ msg

-- compBinOp e1 e2 env seq =
--   let c1 = (compile e1 env) in
--   let c2 = (compile e2 env) in
--   c1 ++ c2 ++ seq

-- compile :: Expr -> CEnvironment -> [Inst]

-- compile (Var x) env =
--   case (elemIndex x env) of
--     (Just a) -> [(IVar a)]
--     Nothing -> compileError $ "Unknown variable '" ++ x ++ "'"
    
-- compile (Num k) env = [(IInt k)]

-- compile (Boolean b) env = [(IBool b)]

-- compile (Times e1 e2) env =
--   compBinOp e1 e2 env [IMult]

-- compile (Divide e1 e2) env =
--   compBinOp e1 e2 env [IDiv]

-- compile (Plus e1 e2) env =
--   compBinOp e1 e2 env [IAdd]

-- compile (Minus e1 e2) env =
--   compBinOp e1 e2 env [ISub]

-- compile (Equal e1 e2) env =
--   compBinOp 
--   (VBool $ (interpret e1 env) == (interpret e2 env))

--compile (Less e1 e2) env =
--  intBinOp e1 e2 env (<) VBool


-- interpret (If e1 e2 e3) env =
--   case (interpret e1 env) of
--     (VBool True) -> (interpret e2 env)
--     (VBool False) -> (interpret e3 env)
--     _ -> runtimeError "Expected boolean type."

-- interpret (Fun n e) env =
--   (VClosure env (Fun n e))


-- interpret (Apply f o) env =
--   let o' = interpret o env in
--   let f' = interpret f env in
--   case f' of
--     (VClosure env (Fun arg expr)) -> (interpret expr ((arg,o'):env))
--     _ -> runtimeError "Expected function."


-- interpret (Pair e1 e2) env =
--   (VPair (interpret e1 env) (interpret e2 env))

-- interpret (Fst e1) env =
--   case (interpret e1 env) of
--     (VPair ca cd) -> ca
--     _ -> runtimeError "Expected pair."

-- interpret (Snd e1) env =
--   case (interpret e1 env) of
--     (VPair ca cd) -> cd
--     _ -> runtimeError "Expected pair."

-- interpret (Rec n e) env =
--   (interpret e ((n, (VClosure env e)):env))

-- interpret Nil env =
--   VNil

-- interpret (Cons e1 e2) env =
--   (VPair (interpret e1 env) (interpret e2 env))

-- interpret (Match e1 e2 x y e3) env =
--   case (interpret e1 env) of
--     VNil -> interpret e2 env
--     (VPair d1 d2) -> interpret e3 ((x,d1):(y,d2):env)
--     _ -> runtimeError "List expected in match."




main =
  let expr = (Apply
              (Fun "x" (Less (Var "x") (Var "x")))
              (Num 2))
  in do
    
    putStrLn $ "Constraints of expr: " ++ (show $ constraintsOf expr [])
    putStrLn $ "Type of expr: " ++ (show $ typeOf expr [])

    putStrLn $ "Value of expr: " ++ (show $ interpret expr []) 
