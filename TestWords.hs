
import Data.List
import Data.Char

import FBackTrack
import Dict
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

pt4 = "a critic is a gong at a railroad crossing clanging loudly and vainly as the train goes by"

main = do
    dd <- getDict
    ss <- getShapeDict
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct1
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct2
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct3
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct4
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct5
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct6
    putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct7
    --putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct8
    --putStrLn $ concat $ intersperse " " $ decrypt dd $ words ct9

    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct1
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct2
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct3
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct4
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct5
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct6
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct7
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct8
    putStrLn $ concat $ intersperse " " $ shapeDecrypt ss $ words ct9

    --cc <- getCryptogram "./data/cryptogram2.txt"
    --putStrLn $ concat $ intersperse " " $ shapeDecrypt dd $ words cc

getCryptogram :: String -> IO String
getCryptogram name = do
    ls<-readFile name
    let wds = concatMap (\s -> if s == [] then " " else s) $ filter (\s-> (length s == 0) || (not $ (isDigit $ head s)||(elem '.' s))) $ lines ls
    return wds

