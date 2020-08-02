module TPS.Classes where

import Prelude
import Halogen (ClassName)
import Tailwind as T

{-
Groups of Tailwind CSS classes
-}
--
data Responsiveness
  = NonMobile
  | MobileOnly
  | RenderAlways

nonMobileBlockClasses :: Array ClassName
nonMobileBlockClasses = [ T.hidden, T.smBlock ]

nonMobileBlock :: Responsiveness -> Array ClassName
nonMobileBlock NonMobile = nonMobileBlockClasses

nonMobileBlock _ = []

commonBgClasses :: Array ClassName
commonBgClasses =
  [ T.bgTpsBlack
  , T.hoverBgBlack
  ]

commonMenuClasses :: Array ClassName
commonMenuClasses =
  [ T.px3
  , T.block
  ]
    <> commonBgClasses

commonTextClasses :: Array ClassName
commonTextClasses =
  [ T.textWhite
  , T.leading10
  ]

menuTextClasses :: Array ClassName
menuTextClasses =
  [ T.borderL
  , T.borderSolid
  , T.borderGray700
  ]
    <> commonMenuClasses
    <> commonTextClasses

dropdownItemClasses :: Array ClassName
dropdownItemClasses =
  [ T.block
  , T.wFull
  ]
    <> commonBgClasses
    <> commonTextClasses
