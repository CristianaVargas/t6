import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)

-- Gera retangulo SVG 
-- a partir de coordenadas+dimensoes e de uma string com atributos de estilo
writeRect :: (String,Rect) -> String 
writeRect (style,((x,y),w,h)) = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- Gera codigo-fonte de arquivo SVG 
-- concatenando uma lista de retangulos e seus atributos de estilo
writeRects :: Float -> Float -> [(String,Rect)] -> String 
writeRects w h rs = 
  printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 
      ++ (concatMap writeRect rs) ++ "</svg>"

{--
     O codigo abaixo cria um arquivo "colors.svg" com 2 retangulos.
     Para simplificar o exemplo, todos os atributos sao definidos manualmente,
     mas para gerar figuras maiores os atributos deverao ser calculados por funcoes 
 --}
main :: IO ()
main = do
  let
	hue = 0	--matiz
	alt = 50
	larg = 80
	cor = 360
	linhas = 9
	colunas = 5
	esp = 5	--espaçamento entre os retangulos
	esp2 = 20
	n = 20		--numero de matizes
	
	style = map(\(h,l,s) -> (printf "fill:hsl("++show h++", "++show l++"% ,"++show s++"% )")) [(z, w, k) | z <- [hue,(hue+30)..((30*(n-1))+hue)], w <- [100,ceiling(100-(100/(colunas-1)))..0], k <- [100,ceiling(100-(100/(linhas-1)))..0]]
	trec =  map(\(c,d,a,b) -> (((a+(colunas*(2*esp+larg)*c)),(b+(colunas*(2*esp+larg+15)*d))),larg,alt)) [(i,j,((x*(larg+esp)+esp)),((y*(alt+esp)+esp))) | i <- [0..((n/2)-1)], j <- [0..((n/2)-1)],  x <- [0..(colunas-1)], y <- [0..(linhas-1)]] 
	rects = zip style trec
	
	(w,h) = (n/2*(2*esp+larg)*colunas,n/2*(2*esp+alt)*linhas)
  writeFile "colorsP2.svg" $ writeRects w h rects
  -- o codigo acima eh equivalente a:
  -- writeFile "colors.svg" (writeRects w h rects)