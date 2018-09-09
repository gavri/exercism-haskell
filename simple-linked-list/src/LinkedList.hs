module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = New a (LinkedList a) | Nil deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "This list is empty!"
datum (New x xs) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x: xs) = New x $ fromList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = New x linkedList

next :: LinkedList a -> LinkedList a
next Nil = error "This list is empty!"
next (New x xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Nil = Nil
reverseLinkedList xs = reverseLinkedListIter xs Nil

reverseLinkedListIter :: LinkedList a -> LinkedList a -> LinkedList a
reverseLinkedListIter Nil acc = acc
reverseLinkedListIter (New x xs) acc = reverseLinkedListIter xs (New x acc)

toList :: LinkedList a -> [a]
toList Nil = []
toList (New x xs) = x : toList xs
