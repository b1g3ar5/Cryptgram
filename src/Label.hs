module Label (
    Label(..)
    , notWord
    , Label.showChar
    , labelIsWord
    , isCipherText
) where

import Data.Char

-- | The Label type has a character (ie. the one in the word), a frequency
-- which is the frequency in (English) text that the word exhibits (ie. 0
-- if it is not the end of a word) and a count which is the numbers of words
-- in the branch.
data Label = Label {lch::Char, llogFreq::Double, lcount::Int}

instance Show Label where
    show (Label ch f n) = (show ch) ++ ", " ++ (show n) ++ ", " ++ (show f)

instance Eq Label where
    (Label _ f1 _) == (Label _ f2 _) = f1 == f2

instance Ord Label where
    compare (Label _ f1 _) (Label _ f2 _) = compare f1 f2

showChar :: Label -> String
showChar (Label ch _ _) = [ch]

-- | This is the -log(frequency) that we use for non words
-- ie e^(-1000) == 0. One in a billion is 20.72
notWord :: Double
notWord = 1000.0

-- | Strings adhere to the convention that plain text is lowercase and cipher text is upper case
isCipherText :: Char -> Bool
isCipherText = isUpper

-- | Checks whether the label is the end of a word, ie. the freq > 0.0
-- or the log frequency < 1000.0
labelIsWord :: Label -> Bool
labelIsWord l = llogFreq l < notWord


