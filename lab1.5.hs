
minList [val] = val
minList(val:rest) = if (val < (minList rest))
					then val
					else minList rest

listSort [val] = [val]
listSort [] = []
listSort (val:rest) = if (val <= (minList rest))
					  then val : (listSort rest)
					  else listSort (rest ++ [val])
					
inS val [] = False
inS x (val:rest) = if (x == val)
					then True
					else inS x rest

norep [] = []
norep [val] = [val]
norep (val:rest) = if ((inS val rest) == True)
					then norep rest
					else val:(norep rest)
					
fixS [] = []
fixS [val] = [val]
fixS (val:rest) = norep (listSort(val:rest))

addtoSet val [] = [val]
addtoSet x (val:rest) = fixS(x:val:rest)

unionS [] [] = []
unionS (val1:rest1) (val2:rest2) = fixS((val1:rest1) ++ (val2:rest2))
					  
interS [] [] = []
interS (val:rest) [] = []
interS [] (val:rest) = []
interS (val1:rest1) (val2:rest2) = if (inS val1 (val2:rest2) == True)
									then fixS (val1:(interS rest1 (val2:rest2)))
									else fixS (interS rest1 (val2:rest2))
		
setEq [] [] = True
setEq [] (val:rest) = False
setEq (val:rest) [] = False
setEq (val1:rest1) (val2:rest2) = if (val1 == val2)
									then setEq rest1 rest2
									else False
									
addAB [] = [] --what about empty string???
addAB [""] = ["ab"]
addAB (val:rest) = [val ++ "ab"] ++ (addAB rest)

abstar =  addAB( addAB([""] ++ abstar))--issues 

