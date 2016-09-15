minList [val] = val
minList(val:rest) = if (val < (minList rest))--recursively get the minimum in the list
					then val
					else minList rest

listSort [val] = [val]
listSort [] = []
listSort (val:rest) = if (val <= (minList rest))
					  then val : (listSort rest)
					  else listSort (rest ++ [val])--if val is not the mininum in the list, move it to the end
					
inS val [] = False
inS x (val:rest) = if (x == val)
					then True
					else inS x rest--if x is not the first value, check it with the rest list

norep [] = []
norep [val] = [val]
norep (val:rest) = if ((inS val rest) == True)
					then norep rest--if first value has already appeared in the rest, take it out
					else val:(norep rest)
					
fixS [] = []
fixS [val] = [val]
fixS (val:rest) = norep (listSort(val:rest))--combination of various fuctions above

addtoSet val [] = [val]
addtoSet x (val:rest) = fixS(x:val:rest)--add it the head of the set and fix it. 

unionS [] [] = []
unionS (val1:rest1) (val2:rest2) = fixS((val1:rest1) ++ (val2:rest2))--combine and fix 
					  
interS [] [] = []
interS (val:rest) [] = []
interS [] (val:rest) = []
interS (val1:rest1) (val2:rest2) = if (inS val1 (val2:rest2) == True)-- if val1 is in both, append it, else move it
									then fixS (val1:(interS rest1 (val2:rest2)))
									else fixS (interS rest1 (val2:rest2))
		
setEq [] [] = True
setEq [] (val:rest) = False
setEq (val:rest) [] = False
setEq (val1:rest1) (val2:rest2) = if (val1 == val2)--check value one by one for both sets
									then setEq rest1 rest2
									else False
									
addAB [] = [] 
addAB [""] = ["a","b"]
addAB (val:rest) = [val ++ "a"] ++ [val ++ "b"] ++ (addAB rest)

deleteHead [] = []
deleteHead (val:rest) = rest--helper function

abstar =  [""] ++ (addAB[""] ++ (addAB (deleteHead abstar)))--need to delete head before adstar again

addABC [] = [] 
addABC [""] = ["a","b","c"]
addABC (val:rest) = [val ++ "a"] ++ [val ++ "b"] ++ [val ++ "c"] ++ (addABC rest)--add 'a' 'b' 'c' to the back of each string in the list

abcstar =  [""] ++ (addABC[""] ++ (addABC (deleteHead abcstar)))--need to delete head before adcstar again

helperAB [] = []
helperAB [""] = ["ab"]
helperAB (val:rest) = ["a" ++ val ++ "b"] ++ (helperAB rest)--help function to add 'a' and 'b' on two sides of each element

listaNbN = [""] ++ (helperAB [""] ++ (helperAB (deleteHead listaNbN)))--use the helper func above to construct listaNbN













