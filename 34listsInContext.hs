-- lists and applicatives

-- putting a function in a context:
test01 = pure (minOfThree) <*> (readInt) <*> (readInt) <*> (readInt)