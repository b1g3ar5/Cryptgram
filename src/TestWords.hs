
module TestWords
    (
      someFunc
    ) where


import Data.List
import Data.Char
import Data.Tree

import FBackTrack
import Dict
import Label
import Shape
import Words
import Crib

ct1 = "SIAUGFC FKJJBU PC VBM BJC AGWC KYBJC PIU HIAU PC VBM PBUO"
ct2 = "DJ JDQ FED MDOQPLKEDO ZJHQ GTJ TEL DJK QBNQPCQDFQO CDXEKMEKCJD EDO DJ JDQ FED MDOQPLKEDO CDXEKMEKCJD DJ VEKKQP TJG VEDS KCVQL TQ TEL QBNQPCQDFQO CK"
ct3 = "BUJOKZ TH KZU SFICLAK IN PJDH AJSJATKE KI KZTDX"
ct4 = "O YNQFQY QH O SPRS OF O NOQCNPOV YNPHHQRS YCORSQRS CPZVCM ORV JOQRCM OH FXW FNOQR SPWH UM"
ct5 = "TGG JDC TLXBVCQJZ JW OLWKC VTQZ ZBOCLHWLHJN RTQQWJ ZDTJJCL JDHZ DTLM ITRJ HQ ZBIICLHQX JDC TQHVTGZ TLC WBL CPBTGZ"
ct6 = "XTXFL JXMXFNEOCM EBOMDV OE BNV EBX NMVGXFV NMY XTXFL JXMXFNEOCM OV BWKUIXY UL MNEWFX"
ct7 = "CDNATD KYY DYVD XD VDDM RBAJ ART HAZZAJ YKCAT KV K JKUQAJ UWD CYDVVQJLV AN KYZQLWUS LAE"
ct8 = "NFVSQWFKS RP F ZSVO RYNLVWFQW NVLBSPPRLQ CAW QL WSPW LB BRWQSPP BLV RW RP SZSV RYNLPSE RQ WIS RQWSVSPW LB WIS UIRXEVSQ"
ct9 = "JBN WNSVEG JBNWN SWN VE RNI RNXSZN UEZOJOKOSGV OV JBSJ OJ OV JEE XQKB JWEQYZN JE UQJ XSFNQU EG JIE RSKNV"
ct10 = "DXKXFDXDDNDFX"

pt4 = "a critic is a gong at a railroad crossing clanging loudly and vainly as the train goes by"


--allWordSplits rest = concatMap (\ix ->
                            --fmap (\wds -> (take (wordLengths!!ix) rest) : wds) $ allWordSplits (drop (wordLengths!!ix) rest)
                          --) $ filter (\i-> (wordLengths!!i) <= n) [0..10]
  --where
--    n = length rest




someFunc = do
  dd <- getDict
  ss <- getShapeDict
  let rest0 = "allthewords"

  let ls = filter (\l -> llogFreq l < 1000.0) $ fmap rootLabel dd
  putStrLn $ show ls

  let wdss = allWordSplits dd rest0
  let okWords = filter (\wds -> foldl (\acc wd -> acc && (isWord dd wd)) True wds) wdss
  --print wdss
  print $ "Length is: " ++ show (length $ wdss)
  print okWords
  print $ "Length is: " ++ show (length $ okWords)

{-

  putStrLn "This is the decrypt funtion:\n"
  putStrLn $ unwords $ decrypt dd $ words ct1
  putStrLn $ unwords $ decrypt dd $ words ct2
  putStrLn $ unwords $ decrypt dd $ words ct3
  putStrLn $ unwords $ decrypt dd $ words ct4
  putStrLn $ unwords $ decrypt dd $ words ct5
  putStrLn $ unwords $ decrypt dd $ words ct6
  putStrLn $ unwords $ decrypt dd $ words ct7
  -- These next 2 don't work - too difficult
  --putStrLn $ unwords $ decrypt dd $ words ct8
  --putStrLn $ unwords $ decrypt dd $ words ct9
  putStrLn $ unwords $ decrypt dd $ words ct10

  putStrLn "This is the shapeDecrypt funtion:\n"
  putStrLn $ unwords $ shapeDecrypt ss $ words ct1
  putStrLn $ unwords $ shapeDecrypt ss $ words ct2
  putStrLn $ unwords $ shapeDecrypt ss $ words ct3
  putStrLn $ unwords $ shapeDecrypt ss $ words ct4
  putStrLn $ unwords $ shapeDecrypt ss $ words ct5
  putStrLn $ unwords $ shapeDecrypt ss $ words ct6
  putStrLn $ unwords $ shapeDecrypt ss $ words ct7
  putStrLn $ unwords $ shapeDecrypt ss $ words ct8
  putStrLn $ unwords$ shapeDecrypt ss $ words ct9
  putStrLn $ unwords $ shapeDecrypt ss $ words ct10
-}

getCryptogram :: String -> IO String
getCryptogram name = do
    ls<-readFile name
    let wds = concatMap (\s -> if null s then " " else s) $ filter (\s-> null s || not (isDigit (head s) || elem '.' s)) $ lines ls
    return wds
