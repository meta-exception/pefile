module Signatures where

import Structures.IMAGE_SECTION_HEADER (name)
import Data.List (isInfixOf)

upx_sign = [85, 80, 88]

checkNameUPX sectionHeader = isInfixOf upx_sign (name sectionHeader)

checkSectionsUPX sectionHeaders = or (map checkNameUPX sectionHeaders)

aplib_sign = [77, 56, 90]

checkDosUPX bytes = isInfixOf aplib_sign bytes