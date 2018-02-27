-- | Standardises spaces between top level declarations
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.BlockSpacing
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Char                     (isSpace)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
blockSpacing :: [String] -> a -> [String]
blockSpacing ls _ = reverse . foldl f [] $ ls
    where
        f ([]:acc) l
            | isTopLevel l = l:[]:[]:dropWhile ([] ==) acc -- Two spaces before a top-level declaration
            | null l = l:acc                               -- Single space everywhere else
            | otherwise = l:[]:acc
        f acc l = l:acc

        isTopLevel (v:_) = not $ isSpace v
        isTopLevel _   = False

--------------------------------------------------------------------------------
step :: Step
step = makeStep "BlockSpacing" blockSpacing
