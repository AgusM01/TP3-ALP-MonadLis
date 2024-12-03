module Eval1
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

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip = return Skip -- Nunca va a ser Skip
stepComm (Let v1 exp) =  do eva <- evalExp exp
                            update v1 eva 
                            return Skip 
stepComm (Seq Skip c2)  = do  sc2 <- stepComm c2
                              return (Seq Skip sc2)
stepComm (Seq c1 c2)    = do  sc1 <- stepComm c1 
                              return (Seq sc1 c2)  
stepComm (IfThenElse eb c1 c2) = do val <- evalExp eb 
                                    if val then return c1 
                                    else return c2
stepComm r@(Repeat eb c) = return (Seq c (IfThenElse eb r Skip)) 
      
-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
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
                            return (div v1 v2)
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


