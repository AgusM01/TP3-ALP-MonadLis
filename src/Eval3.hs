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
                                Just x -> Right ((x :!: s) :!: "Found " ++ v ++ " = " ++ (show x) ++ "; ") 
                                Nothing -> runStateErrorTrace (throw UndefVar) s) 
                    where lookfor' v s = M.lookup v s
    update v i = StateErrorTrace (\s -> Right ((() :!: update' v i s) :!: "Updating " ++ v ++ " = " ++ (show i) ++ "; ")) 
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
stepComm Skip =  return Skip
stepComm (Let var exp) = do eva <-  evalExp exp 
                            addTrace ("Let " ++ var ++ " = " ++ (show eva) ++ "; ")
                            update var eva 
                            return Skip
stepComm (Seq Skip c2)  = return c2 
stepComm (Seq c1 c2)    = do  sc1 <- stepComm c1 
                              return (Seq sc1 c2)  
stepComm (IfThenElse eb c1 c2) = do val <- evalExp eb 
                                    if val then return c1 
                                    else return c2
stepComm r@(Repeat eb c) = return (Seq c (IfThenElse eb r Skip)) 


-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var v)   = do  val <- lookfor v 
                        return val 
evalExp (UMinus e) = do val <- evalExp e 
                        return (-val)
evalExp (Plus e1 e2)  = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            return (v1 + v2)
evalExp (Minus e1 e2)  = do v1 <- evalExp e1
                            v2 <- evalExp e2 
                            return (v1 - v2)
evalExp (Times e1 e2)  = do v1 <- evalExp e1
                            v2 <- evalExp e2 
                            return (v1 * v2)
evalExp (Div e1 e2)  = do   v1 <- evalExp e1
                            v2 <- evalExp e2
                            if v2 == 0 then throw DivByZero 
                            else return (div v1 v2)
evalExp (BTrue)       = do  return True 
evalExp (BFalse)      = return False
evalExp (Lt e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            return (v1 < v2)
evalExp (Gt e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            return (v1 > v2)
evalExp (And e1 e2)    = do v1 <- evalExp e1
                            v2 <- evalExp e2
                            return (v1 && v2)
evalExp (Or e1 e2)    = do  v1 <- evalExp e1
                            v2 <- evalExp e2
                            return (v1 || v2)
evalExp (Not e) = do  e <- evalExp e
                      return (not e)
evalExp (Eq e1 e2) = do v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (v1 == v2)
evalExp (NEq e1 e2) = do  v1 <- evalExp e1
                          v2 <- evalExp e2
                          return (v1 /= v2)
evalExp (IncVar v) = do val <- lookfor v
                        update v (val + 1)
                        return (val + 1)
evalExp (DecVar v) = do val <- lookfor v
                        update v (val - 1) 
                        return (val - 1)
