module Logger where

printList xs = do
  mapM (print) xs

padList xs len el =
  if length(x) >= len
    then x
    else padList (x ++ el) len