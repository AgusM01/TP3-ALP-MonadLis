module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case (runStateError m) s of
                               Right (a :!: s') -> runStateError (f a) s'
                               Left e -> Left e)
                            

-- MonadError es una sub-clase de la clase Monad.

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where 
  throw e =  StateError (\s -> Left e)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case (lookfor' v s) of 
                              Just x -> Right (x :!: s)
                              Nothing -> runStateError (throw UndefVar) s) 
                    where lookfor' v s = M.lookup v s
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert


-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error Env
eval c = case (runStateError (stepCommStar c) initEnv) of 
            Right x   -> Right (snd x)
            Left err  -> Left err   

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm Skip = return Skip 
stepComm (Let var exp) = do eva <- evalExp exp 
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const a) = return a
evalExp (Var v)   = do  val <- lookfor v -- Ahora si no lo encuentra tira el error.
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
evalExp (BTrue)       = return True 
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
