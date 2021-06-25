-- exercicio 1
-- parametros: F e C
-- retorna R
impedancia :: (Floating a) => a -> a -> a
impedancia f c = 1 / (2 * pi * f * c)

-- exercicio 2 e 6 
-- parametros: F, R, C e Ag
-- retorna Vrmax
vrmax :: (Floating a) => a -> a -> a -> a -> a
vrmax f r c ag = (2 * pi * f * r * c * ag) / (sqrt (1 + (2 * pi *f * r * c)^2))

-- exercicio 4
-- parametros: F, R, C e Ag
-- retorna Vcmax
vcmax :: (Floating a) => a -> a -> a -> a -> a
vcmax f r c ag = ag / (sqrt (1 + (2 * pi *f * r * c)^2))

-- exercicio 5                 
-- parametros: R C
-- retorna constante
constante :: (Floating a) => a -> a -> a
constante r c = r * c

-- exercicio 3
-- parametros: R C VM e VC  (dentro do logaritmo, aquilo que queremos sobre aquilo que temos)
-- retorna t
quantoDemora :: (Floating a) => a -> a -> a -> a -> a
quantoDemora r c vm vc = (-r * c * logBase (exp 1) (vm / vc))
