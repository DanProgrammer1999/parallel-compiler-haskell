module Tree where

data Tree a = Tree
    { treeRoot     :: a
    , treeChildren :: [Tree a]
    }

instance Show a => Show (Tree a) where
    show = showT 0
        where
            showT n (Tree node children)
                =  nodePrefix n ++ show node
                ++ (if null children then " " else "\n" ++ levelPrefix ' ' n)
                ++ "[" ++ concatMap (("\n" ++) . showT (n + 1)) children ++ "]\n"
                ++ levelPrefix ' ' (n - 1)

            levelPrefix c n = replicate (n * 4) c
            nodePrefix 0 = ""
            nodePrefix n = init (levelPrefix '.' n) ++ " "

instance Foldable Tree where
  foldr f b (Tree node children) = foldChildren (f node b) children
    where
        foldChildren res [] = res
        foldChildren curr (n : ns) = foldChildren (foldr f curr n) ns

instance Functor Tree where
  fmap f (Tree node children) = Tree (f node) (map (fmap f) children)

data ASTNode = ASTNode
    { nodeType  :: String
    , nodeValue :: String
    }

instance Show ASTNode where
  show (ASTNode nType nVal) = nType ++ "(" ++ nVal ++ ")"

type AST = Tree ASTNode

treeToList :: AST -> [ASTNode]
treeToList = reverse . foldr (:) []

treeSize :: AST -> Int
treeSize = calcTreeSize 1
    where
        calcTreeSize count (Tree _ children) = count + sum (map (calcTreeSize count) children)

astLeafNode :: String -> String -> AST
astLeafNode nType nVal = Tree (ASTNode nType nVal) []