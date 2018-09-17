{-# LANGUAGE LambdaCase, OverloadedLabels, OverloadedLists, OverloadedStrings,ScopedTypeVariables, UnicodeSyntax #-}
module Main where
{- import           Control.Applicative.Unicode
import           Control.Arrow.Unicode
import qualified Control.Category.Unicode      as Category
import           Control.Monad.Unicode
import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.Foldable.Unicode
import           Data.Function.Unicode
import           Data.List.Unicode
import           Data.Monoid.Unicode
import           Data.Ord.Unicode
import           Prelude.Unicode
 -}
import           Control.Monad
import           GI.Gtk                  hiding ( main )
import           GI.Vte
import           GI.Gio                         ( Cancellable )
import           GI.Gdk
import           System.Exit

color256to1 :: Int -> Double
color256to1 i = (fromIntegral i) / 256.0

main :: IO ()
main = applicationNew (Just "cn.Magicloud.Thirm") [] >>= \case
  Nothing  -> die "cannot create application"
  Just app -> do
    void $ app `on` #activate $ do
      win <- new ApplicationWindow [#title := "Thirm"]
      #addWindow app win
      term <- new Terminal []
      #add win term
      #showAll term
      solarizedLight <- mapM
        (\(r, g, b) -> new
          RGBA
          [ #red := color256to1 r
          , #green := color256to1 g
          , #blue := color256to1 b
          , #alpha := 1
          ]
        )
        [ (7  , 54 , 66)
        , (220, 50 , 47)
        , (133, 153, 0)
        , (181, 137, 0)
        , (38 , 139, 210)
        , (211, 54 , 130)
        , (42 , 161, 152)
        , (238, 232, 213)
        , (0  , 43 , 54)
        , (203, 75 , 22)
        , (88 , 110, 117)
        , (101, 123, 131)
        , (131, 148, 150)
        , (108, 113, 196)
        , (147, 161, 161)
        , (253, 246, 227)
        ]
      #setColors term (Just $ solarizedLight !! 11) (Just $ solarizedLight !! 15) $ Just solarizedLight
      void $ term `on` #childExited $ \_ -> #close win
      sh <- getUserShell
      void $ #spawnSync term
                        []
                        Nothing
                        [sh]
                        Nothing
                        []
                        Nothing
                        (Nothing :: Maybe Cancellable)
      #present win
    #run app Nothing >>= \case
      0  -> exitSuccess
      ec -> exitWith $ ExitFailure $ fromIntegral ec
