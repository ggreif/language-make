{-# LANGUAGE ViewPatterns, GADTs #-}

module Expansion (Expansion(..), expansion, interpr, accum) where

data Eater = Toplev { prepender :: Eater -> Char -> Eater, accum :: Expansion String }
           | Deeper { prepender :: Eater -> Char -> Eater, accum :: Expansion String, cont :: Eater, collapse :: Eater -> Eater }

expansion :: String -> [Char] -> Eater
expansion [] [] = Toplev {prepender = prepend, accum = String ""}
expansion [] ps = error $ "Non-matched closing parens: " ++ show ps
expansion ('$':c@'$':cs) (expansion cs -> eater) = dup prepender eater c
expansion (c:cs) (((==c) -> True):ctx) = Deeper{cont = expansion cs ctx, prepender = prepend, collapse = collapse, accum = String ""}
  where collapse Deeper{accum=a, cont=cont} = cont{accum=a `Subst` Lookup `Then` accum cont}
expansion ('$':(closing -> Just p):cs) (expansion cs . (p:) -> eater) = dup collapse eater
expansion ('$':c:rest) (expansion ('$':'(':c:')':rest) -> eater) = eater
expansion "$" (expansion "$()" -> eater) = eater -- curiously looking up an empty var name is legal, defining is not!
expansion (c:cs) (expansion cs -> eater) = dup prepender eater c

closing p = lookup p (zip "({" ")}")
dup f a = f a a
prepend t c = go t c (accum t)
  where go t c (String cs) = t{ accum = String (c : cs) }
        go t c (String cs `Then` more) = t{accum = String (c : cs) `Then` more}
        go t c more = t{accum = String (c:[]) `Then` more}

data Expansion a where
  Filter :: String -> Expansion a -> Expansion a
  String :: String -> Expansion a
  Then :: Expansion b -> Expansion b -> Expansion b
  Subst :: Expansion b -> (b -> Expansion b) -> Expansion b
  -- Error :: String -> Expansion a -- fail?
  Lookup :: String -> Expansion String

infixr 5 `Then`

instance  (a ~ String, Show a) => Show (Expansion a) where
  show (Lookup a) = "Lookup:" ++ a
  show (String a) = show a
  show (a `Then` b) = "(" ++ show a ++ " `Then` " ++ show b ++ ")"
  show (a `Subst` f) = "(" ++ show a ++ " >>= <fn>[i.e." ++ show (f "{}") ++ "])"

interpr :: Expansion String -> String
interpr (expr `Subst` f) = interpr (f (interpr expr))
interpr (e `Then` f) = interpr e ++ interpr f
interpr (Lookup s) = "{" ++ s ++ "}"
interpr (String a) = a

