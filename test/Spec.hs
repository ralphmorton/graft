{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Graft

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T

main :: IO ()
main = hspec templating

templating :: Spec
templating = do
    describe "Graft" $ do
        mapM_ (uncurry testCOmpilation) testTpls

testCOmpilation :: T.Text -> T.Text -> Spec
testCOmpilation from to = do
    let desc = mconcat ["it should transform\n\n", from, "\n\nto\n\n", to]
    it (T.unpack desc) $ do
        let res = attachTemplate mempty "test" from
        case res of
            Left e -> expectationFailure e
            Right grft -> case (compile grft "test") of
                Right t | t == to -> return ()
                err -> expectationFailure (show err)

compile :: GraftData -> String -> Either GraftError T.Text
compile grft name = runExcept $ runReaderT (graft name testData) grft

testTpls :: [(T.Text, T.Text)]
testTpls =
    [
        (t1, t1'),
        (t2, t2'),
        (t3, t3'),
        (t4, t4'),
        (t5, t5'),
        (t6, t6')
    ]

testData :: M.Map T.Text Var
testData =
    M.fromList $ [
        ("val", Val "val"),
        ("array", testArray),
        ("object", testObject)
    ]

testArray :: Var
testArray =
    Array $ [
        (M.fromList [("v", Val "v1")] :: M.Map T.Text Var),
        (M.fromList [("v", Val "v2")] :: M.Map T.Text Var),
        (M.fromList [("v", Val "v3")] :: M.Map T.Text Var)
    ]

testObject :: Var
testObject =
    Object . M.fromList $ [
        ("o1" :: T.Text, Val "o1val"),
        ("o2" :: T.Text, testArray),
        ("o3" :: T.Text, testNestedObject)
    ]

testNestedObject :: Var
testNestedObject =
    Object . M.fromList $ [
        ("no1" :: T.Text, Val "no1val"),
        ("no2" :: T.Text, Val "no2val")
    ]

t1 :: T.Text
t1 = "{{val}}"

t1' :: T.Text
t1' = "val"

t2 :: T.Text
t2 = "[[ x:array ]]{{x.v}}[[ end ]]"

t2' :: T.Text
t2' = "v1v2v3"

t3 :: T.Text
t3 = "[[x:array]][[y:array]]{{x.v}}{{y.v}}[[end]][[end]]"

t3' :: T.Text
t3' = "v1v1v1v2v1v3v2v1v2v2v2v3v3v1v3v2v3v3"

t4 :: T.Text
t4 = "[[?val]]{{val}}[[end]]"

t4' :: T.Text
t4' = "val"

t5 :: T.Text
t5 = "[[ ?val2 ]]{{val2}}[[ end ]]"

t5' :: T.Text
t5' = ""

t6 :: T.Text
t6 = "{{object.o1}}[[v:object.o2]]{{v.v}}[[end]]{{object.o3.no1}}"

t6' :: T.Text
t6' = "o1valv1v2v3no1val"
