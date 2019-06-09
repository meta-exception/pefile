module Logger where

printList xs = do
  mapM (print) xs

padList xs count elms =
  if count <= 0
    then xs
    else padList (xs ++ elms) (count-1) elms

{-|
instance Show IMAGE_DATA_DIRECTORY where
  show (IMAGE_DATA_DIRECTORY virtualAddress size) =
    printf (padList "[%s]\n" 2 "%-20s\t%#x\n")
      "IMAGE_DATA_DIRECTORY"
       "VirtualAddress:" virtualAddress
       "Size:" size
-}

{-|
instance Show IMAGE_DOS_HEADER where
  show (IMAGE_DOS_HEADER e_magic e_cblp e_cp e_crlc e_cparhdr e_minalloc e_maxalloc e_ss e_sp e_csum e_ip e_cs e_lfarlc e_ovno e_res e_oemid e_oeminfo e_res2 e_lfanew) =
    printf (padList "[%s]\n" 14 "%-20s:\t%#x\n")
      "IMAGE_DOS_HEADER"
       "e_magic" e_magic
       "e_cblp" e_cblp
       "e_cp" e_cp
       "e_crlc" e_crlc
       "e_cparhdr" e_cparhdr
       "e_minalloc" e_minalloc
       "e_maxalloc" e_maxalloc
       "e_ss" e_ss
       "e_sp" e_sp
       "e_csum" e_csum
       "e_ip" e_ip
       "e_cs" e_cs
       "e_lfarlc" e_lfarlc
       "e_ovno" e_ovno ++
      printf "%-20s:\t%s\n" "e_res" (show e_res) ++
      printf (padList "" 2 "%-20s:\t%#x\n")
       "e_oemid" e_oemid
       "e_oeminfo" e_oeminfo ++
      printf "%-20s:\t%s\n" "e_res2" (show e_res2) ++
      printf "%-20s:\t%#x\n"
       "e_lfanew" e_lfanew
-}

{-|
instance Show IMAGE_DOS_STUB where
  show (IMAGE_DOS_STUB bytes) =
    printf "[%s]\n%s\n"
      "IMAGE_DOS_STUB"
      (show bytes)
-}

{-|
instance Show IMAGE_EXPORT_DIRECTORY where
  show (IMAGE_EXPORT_DIRECTORY characteristics timeDateStamp majorVersion minorVersion name base numberOfFunctions numberOfNames addressOfFunctions addressOfNames addressOfNameOrdinals) =
    printf (padList "[%s]\n" 11 "%-20s:\t%#x\n")
      "IMAGE_EXPORT_DIRECTORY"
       "Characteristics" characteristics
       "TimeDateStamp" timeDateStamp
       "majorVersion" majorVersion
       "minorVersion" minorVersion
       "name" name
       "base" base
       "numberOfFunctions" numberOfFunctions
       "numberOfNames" numberOfNames
       "addressOfFunctions" addressOfFunctions
       "addressOfNames" addressOfNames
       "addressOfNameOrdinals" addressOfNameOrdinals
-}

{-|
instance Show IMAGE_RICH_SIGN where
  show (IMAGE_RICH_SIGN bytes) =
    printf "[%s]\n%s\n"
      "IMAGE_RICH_SIGN"
      (show bytes)
-}
