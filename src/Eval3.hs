module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva mónada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
    StateErrorTrace { runStateErrorTrace :: Env -> Either Error (Pair (Pair a Env) String) }

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
   fmap = liftM
 
instance Applicative StateErrorTrace where
   pure  = return
   (<*>) = ap

instance Monad StateErrorTrace  where
    return x = StateErrorTrace (\s -> Right ((x :!: s) :!: []))
    m >>= f = StateErrorTrace (\s ->    case  runStateErrorTrace m s of 
                                        Right ((v :!: s') :!: t) -> (case runStateErrorTrace (f v) s' of 
                                                                        Right ((nv :!: s'') :!: t') -> Right ((nv :!: s'' :!: t ++ t'))
                                                                        Left e -> Left e)
                                        Left e -> Left e)


-- Ejercicio 3.b: Resolver en Monad.hs
-- COMPLETAR

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where 
    addTrace t = StateErrorTrace (\s -> Right ((():!:s):!: t)) 

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where 
    throw e = StateErrorTrace (\s -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where 
    lookfor v = StateErrorTrace (\s -> case (lookfor' v s) of 
                                Just x -> Right ((x :!: s) :!: v ++ "=" ++ (show x) ++ "\n") 
                                Nothing -> runStateErrorTrace (throw UndefVar) s) 
                    where lookfor' v s = M.lookup v s
    update v i = StateErrorTrace (\s -> Right ((() :!: update' v i s) :!: v ++ "=" ++ (show i) ++ "\n")) 
                    where update' = M.insert
-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval c = case (runStateErrorTrace (stepCommStar c) initEnv) of
            Right ((val :!: env) :!: tr) -> Right (env , tr)
            Left err -> Left err

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm  
stepComm Skip = do addTrace "Skip\n" 
                   return Skip 
stepComm (Let var exp) = do eva <-  evalExp exp 
                            addTrace ("Let" ++ var ++ "=" ++ (show eva) ++ "\n")
                            update var eva 
                            return Skip
stepComm (Seq c1 c2) = do   sc1 <- stepComm c1 
                            sc2 <- stepComm c2
                            addTrace "Seq \n"
                            return (Seq sc1 sc2)
stepComm (IfThenElse eb c1 c2) = do val <- evalExp eb
                                    addTrace ("IfThenElse with value: " ++ show val ++ "\n")
                                    if val then (return c1)
                                    else (return c2)
stepComm (Repeat eb c) = do val <- evalExp eb
                            addTrace ("Repeat with value: " ++ show val ++ "\n")
                            if val then (return (Repeat eb c))
                            else return Skip 


-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const a) = do  addTrace ("Const " ++ (show a) ++ "\n")
                        return a
evalExp (Var v)   = do  val <- lookfor v -- Ahora si no lo encuentra tira el error.
                        return val 
evalExp (UMinus e) = do val <- evalExp e 
                        return (-val)
evalExp (Plus e1 e2)  = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ "+" ++ (show v2) ++ "\n") 
                            return (v1 + v2)
evalExp (Minus e1 e2)  = do v1 <- evalExp e1
                            v2 <- evalExp e2 
                            addTrace ((show v1) ++ "-" ++ (show v2) ++ "\n") 
                            return (v1 - v2)
evalExp (Times e1 e2)  = do v1 <- evalExp e1
                            v2 <- evalExp e2 
                            addTrace ((show v1) ++ "*" ++ (show v2) ++ "\n") 
                            return (v1 * v2)
evalExp (Div e1 e2)  = do   v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ "/" ++ (show v2) ++ "\n") 
                            if v2 == 0 then throw DivByZero 
                            else return (div v1 v2)
evalExp (BTrue)       = do  addTrace "True" 
                            return True 
evalExp (BFalse)      = do  addTrace "False"
                            return False
evalExp (Lt e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ "<" ++ (show v2) ++ "\n") 
                            return (v1 < v2)
evalExp (Gt e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ ">" ++ (show v2) ++ "\n") 
                            return (v1 > v2)
evalExp (And e1 e2)    = do v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ "&&" ++ (show v2) ++ "\n") 
                            return (v1 && v2)
evalExp (Or e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            addTrace ((show v1) ++ "||" ++ (show v2) ++ "\n") 
                            return (v1 || v2)
evalExp (Not e) = do  e <- evalExp e
                      addTrace ("Not" ++ show e ++ "\n")
                      return (not e)
evalExp (Eq e1 e2) = do v1 <- evalExp e1
                        v2 <- evalExp e2
                        addTrace ((show v1) ++ "==" ++ (show v2) ++ "\n") 
                        return (v1 == v2)
evalExp (NEq e1 e2) = do  v1 <- evalExp e1
                          v2 <- evalExp e2
                          addTrace ((show v1) ++ "/=" ++ (show v2) ++ "\n") 
                          return (v1 /= v2)
evalExp (IncVar v) = do val <- lookfor v
                        addTrace ("Inc. Var: " ++ v ++ "\n")
                        update v (val + 1)
                        return (val + 1)
evalExp (DecVar v) = do val <- lookfor v
                        addTrace ("Dec. Var: " ++ v ++ "\n")
                        update v (val - 1) 
                        return (val - 1)
