module TestErf(erfc) where

import Erf

import Coconut.BaseTypes
import ISA.PowerISA
-- import ISA.PowerInterp
-- import TestUtils

--just remove idSim
erfc :: Double -> Double
erfc x = head $ floats @Interp $ fst $ erfcSP (unfloats4 x, unfloats4 x)

-- erfT1 = timeStamp $ worstError $
--         map (\x -> (zipWith (\x y -> (x-y)/y) 
--                             (floats @Interp $ fst $ erfSP $ (unfloats x,unfloats x)) 
--                             (error "don't have erf") 
--                    )
--             ) 
--   [[i*scale, scale*i*log ((1 - 0.000001)), scale*i*(log ((1+2**(-16)))), scale*i*(log (1.5))]
--   |i<-[-1000..1000]
--   ]
--   where scale = 0.001

-- erfD1 = fmat (\ x -> fst $ erfSP (x,x)) (id) 1 (1/2) $ [i/100 | i <- [0..383]]

-- erfcD1 = sequence [fmat (\ x -> fst $ erfcSP (x,x)) (\ y -> -y^2 + 
--                 (-25.2973534259259430219669245544+1/y^2*
--                     ( -98.2411738989013083125580162701+1/y^2*
--                         ( -72.2382518991259205095525638505+1/y^2*(-26.3134760165222732045927905721))
--                 )) / 
--                 (1/y^2*12.6486769282300100450301612357+1/y^4*45.5007367784947022354477654387+1/y^6*19.9364703451725214910908865657)) 
--     1 (1/2) 
--     $ concatMap (floats @Interp) $ map unwrds  
--                  [[0x40755130,0xB6E28C99,0xBD62F093,0xBE90A196]
--                  ,[0xBF5CFCA7,0xBAE29092,0xBB62DFD4,0xBE02A89C]
--                  ,[0xBEFFFC57,0xBF7FE7B8,0xBFFFFCB0,0xC002B424]
--                  ,[0x3F742741,0x3FF6DF14,0x40755C48,0x409F7B02]] -- 
--     ,putStrLn $ show  [[1,-7.618469e-06,-6.245417e-02,-3.104686e-01]
--                       ,[-7.778351e-01,-1.950460e-03,-3.906238e-03,-1.431995e-01]
--                       ,[ -5.204753e-01 ,-8.425469e-01 , -9.953202e-01, -9.961251e-01]
--                       ,[1.774111e-01,6.380284e-03,5.901782e-08,1.813660e-12]
--                       ]
--     ]

-- erfDbg eightFloats = 
--   let
--     (input : rest) = snd $ erfcSPDev $ (unfloats $ take 4 eightFloats, unfloats $ drop 4 eightFloats)
--     answer1 = ("Haskell fun1", unfloats $ map (\ y -> -y^2 + 
--                 (-25.2973534259259430219669245544+1/y^2*
--                     ( -98.2411738989013083125580162701+1/y^2*
--                         ( -72.2382518991259205095525638505+1/y^2*(-26.3134760165222732045927905721))
--                 )) / 
--                 (1/y^2*12.6486769282300100450301612357+1/y^4*45.5007367784947022354477654387+1/y^6*19.9364703451725214910908865657))  $ floats @Interp (unfloats $ take 4 eightFloats))
--     answer2 = ("Haskell fun2", unfloats $ map (\ y -> -y^2 + 
--                 (-25.2973534259259430219669245544+1/y^2*
--                     ( -98.2411738989013083125580162701+1/y^2*
--                         ( -72.2382518991259205095525638505+1/y^2*(-26.3134760165222732045927905721))
--                 )) / 
--                 (1/y^2*12.6486769282300100450301612357+1/y^4*45.5007367784947022354477654387+1/y^6*19.9364703451725214910908865657))  $ floats @Interp (unfloats $ drop 4 eightFloats))
--   in 
--     putStrLn $ unlines $ map debugOutS (input : rest ++ [answer1] ++ [answer2])
