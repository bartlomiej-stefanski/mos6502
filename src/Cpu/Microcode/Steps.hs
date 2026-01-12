module Cpu.Microcode.Steps where

import Clash.Prelude

data IMaybe (isJust :: Bool) a where
  INothing :: IMaybe False a
  IJust :: a -> IMaybe True a

deriving instance (Show a) => Show (IMaybe isJust a)

fromIMaybe :: IMaybe free a -> Maybe a
fromIMaybe INothing = Nothing
fromIMaybe (IJust x) = Just x

class Impossible where
  impossible :: a

type family Compat post1 pre2 where
  Compat True True = (TypeError (Text "Conflict between postamble and next preamble"), Impossible)
  Compat post1 pre2 = (() :: Constraint)

data Step pre a hasPre hasPost where
  Step :: IMaybe hasPre pre -> a -> IMaybe hasPost pre -> Step pre a hasPre hasPost

deriving instance (Show a, Show pre) => Show (Step pre a hasPre hasPost)

data Steps pre a hasPre hasPost where
  One :: Step pre a hasPre hasPost -> Steps pre a hasPre hasPost
  More :: (Compat hasPost1 hasPre2) => Step pre a hasPre1 hasPost1 -> Steps pre a hasPre2 hasPost2 -> Steps pre a hasPre1 hasPost2

instance (Show a, Show pre) => Show (Steps pre a hasPre hasPost) where
  show (One p) = show p <> "\n"
  show (More a b) = show a <> show b

step :: IMaybe hasPre pre -> a -> IMaybe hasPost pre -> Steps pre a hasPre hasPost
step hasPre x hasPost = One $ Step hasPre x hasPost

infixr 5 >++>

(>++>) ::
  (Compat hasPost1 hasPre2) =>
  Steps pre a hasPre1 hasPost1 ->
  Steps pre a hasPre2 hasPost2 ->
  Steps pre a hasPre1 hasPost2
One x >++> ys = More x ys
More x xs >++> ys = More x $ xs >++> ys

stepsOf :: Steps pre a post hasPre hasPost -> (Maybe pre, [(a, Wedge pre)])
stepsOf xs = case go xs of (hasPre, ys) -> (fromIMaybe hasPre, ys)
  where
    go :: Steps pre a post hasPre hasPost -> (IMaybe hasPre pre, [(a, Wedge pre)])
    go (One (Step pre x post)) =
      (pre, [(x, wedgeLeft $ fromIMaybe post)])
    go (More (Step pre x post) xs) =
      let (pre', ys) = go xs
          combined = case (post, pre') of
            (INothing, pre) -> wedgeRight $ fromIMaybe pre
            (post, INothing) -> wedgeLeft $ fromIMaybe post
            (IJust {}, IJust {}) -> impossible
       in (pre, (x, combined) : ys)
