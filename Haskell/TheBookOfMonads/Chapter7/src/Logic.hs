module Logic where

import Alternative

type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quiqye", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels = [("Alejandro", "Quiqye"), ("Elena", "Quiqye"), ("John", "Mary"), ("John", "Tom"), ("Mary", "Tim")]

gpgRels :: [(Person, Person)]
gpgRels = do
    (grandp, parent) <- pcRels
    (parent', grandc) <- pcRels
    guard (parent == parent')
    return (grandp, grandc)

siblingRels :: [(Person, Person)]
siblingRels = do
    (parent, a) <- pcRels
    (parent', b) <- pcRels
    guard (parent' == parent && a /= b)
    return (a, b)

sums :: [Int] -> [(Int, Int, Int)]
sums ns = do
    x <- ns
    y <- ns
    z <- ns
    guard (x + y == z)
    return (x, y, z)

pyts :: [Int] -> [(Int, Int, Int)]
pyts ns = do
    x <- ns
    y <- ns
    z <- ns
    guard (x*x + y*y == z*z)
    return (x, y, z)

triples ns = sums ns <|> pyts ns

-- list monad not fair in >>=, will hang on z with infinite list
