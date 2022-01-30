module Stack where

newtype Stack a = Stack {
    items :: [a]
}

stackNew = Stack [] -- Stack { items = [] }
stackPush (Stack items) x = Stack (x:items) 
stackPeek (Stack items) = head items 
stackPop (Stack items) = Stack (tail items) 
