module Parser.Sections where

import Structures.IMAGE_SECTION_HEADER
     ( IMAGE_SECTION_HEADER, image_section_header_bytes, getSectionHeaders
     )
import Structures
     ( sectionEntropy
     )
import System.IO.MMap (mmapFileByteString)
import Data.Serialize.Get (runGet)

data SECTION_HEADER = SECTION_HEADER {
  section :: IMAGE_SECTION_HEADER,
  entropy :: Double
} deriving (Show)

loadSectionEntropy path fileAlignment section = do
  entropy <- sectionEntropy path section fileAlignment
  return (SECTION_HEADER section entropy)

loadSectionsEntropy path fileAlignment sections = do
  sectionsWithEntropy <- mapM (loadSectionEntropy path fileAlignment) sections
  return sectionsWithEntropy

loadSectionHeaders path fileAlignment offset count = do
  bs <- mmapFileByteString path (Just (offset, image_section_header_bytes * count))
  case runGet (getSectionHeaders count) bs of
    Left err -> return []
    Right headers -> do
      sections <- loadSectionsEntropy path fileAlignment headers
      return sections