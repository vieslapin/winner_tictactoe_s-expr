module Gintis
where
import Data.Char
message :: String
message = "(l (m \"x\" 0 \"y\" 0 \"v\" \"x\") (m \"x\" 0 \"y\" 1 \"v\" \"x\") (m \"x\" 0 \"y\" 2 \"v\" \"x\") )"

winner :: Maybe String
winner = winner' (parse message)


winner' :: [(Char,Char,Char)] -> Maybe String
winner' moves
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='1' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='0' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='0' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='1' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='2' && c=='x') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves)/=[]))= Just "Laimejo x"
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)/=[]))= Just "Laimejo o"
	| (((filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)/=[]) && ((filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)/=[]))= Just "Laimejo o"
	
	
	| otherwise = Nothing


parse :: String -> [(Char,Char,Char)]
parse "" = error "no list"
parse ('(' : 'l' : ' ' :rest) = parse' (take ((length rest) -1) rest)
parse _ = error "Invalid type"

parse' :: String -> [(Char,Char,Char)]
parse' "" = []
parse' a = parse'' a []

parse'' :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
parse'' a [] = 
    let
		x = take 1 (drop 7 a)
		x' = head x
		y = take 1 (drop 13 a)
		y' = head y
		v = take 1 (drop 20 a)
		v' = head v
		rest = drop 24 a
	in parse'' rest ((x',y',v') : [])
parse'' ('(':a) acc =
	let
		x = take 1 (drop 6 a)
		x' = head x
		y = take 1 (drop 12 a)
		y' = head y
		v = take 1 (drop 19 a)
		v' = head v
		rest = drop 23 a
	in parse'' rest ((x',y',v'):acc)
parse'' "" acc = acc
