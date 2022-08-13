module EnumOdd where

data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd n) = Odd $ n + 2
  pred (Odd n) = Odd $ n - 2

  toEnum n = Odd $ toInteger n
  fromEnum (Odd n) = fromInteger n

  enumFrom (Odd frm) = map Odd [frm, frm + 2 ..]
  enumFromThen (Odd frm) (Odd thn) = map Odd [frm, thn ..]

  enumFromTo (Odd frm) (Odd to) = map Odd [frm, frm + 2 .. to]
  enumFromThenTo (Odd frm) (Odd thn) (Odd to) = map Odd [frm, thn .. to]

{-

              Tests

-}

-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1, testVal 3, testVal 5, testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 =
  take 9 [testVal 1 .. testVal 7]
    == [testVal 1, testVal 3, testVal 5, testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7 .. testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 =
  take 4 [testVal 1, testVal 5 ..]
    == [testVal 1, testVal 5, testVal 9, testVal 13]
-- -- По убыванию
test6 =
  take 4 [testVal 5, testVal 3 ..]
    == [testVal 5, testVal 3, testVal 1, testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 =
  [testVal 1, testVal 5 .. testVal 11] == [testVal 1, testVal 5, testVal 9]
-- -- По убыванию
test8 =
  [testVal 7, testVal 5 .. testVal 1]
    == [testVal 7, testVal 5, testVal 3, testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 ..] == replicate 4 (testVal 5)
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == replicate 4 (testVal 5)
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 = fromEnum (Odd 3) + 1 == fromEnum (Odd 5)

testList =
  [ test0
  , test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
  , test16
  , test17
  ]
allTests = zip [0 ..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests
