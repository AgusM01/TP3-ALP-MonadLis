module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
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
eval = undefined

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
stepComm = undefined

-- Evalua una expresion
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp = undefined

