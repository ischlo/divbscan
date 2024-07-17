# saved bboxes
library(rlist)

bbox_of_interest = list(
  'ny'= list('bbox'=c('west'=-74.25884
                      ,'east'=-73.70023
                      ,'south'=40.47658
                      ,'north'= 40.91763 )
             ,src = 'data/osm_extracts/us-northeast-latest.osm.pbf')
  
  ,'boston'= list('bbox'=c('west'=-71.2127
                           ,'east'=-70.9614
                           ,'south'=42.2880
                           ,'north'= 42.4513 )
                  ,src = 'data/osm_extracts/us-northeast-latest.osm.pbf')
  
  ,'sf'= list('bbox'=c('west'=-122.5484
                       ,'east'=-121.7313
                       ,'south'=37.1953
                       ,'north'=37.8467)
              ,src='data/osm_extracts/norcal-latest.osm.pbf')
  
  ,'seattle' = list('bbox'=c('west'=-122.4220
                             ,'east'=-122.0938
                             ,'south'=47.5051
                             ,'north'= 47.7416)
                    ,src= 'data/osm_extracts/washington-latest.osm.pbf')
  
  ,'london' =list('bbox'=c('west'=-0.5466
                           ,'east'=0.3008
                           ,'south'=51.2559
                           ,'north'= 51.7202)
                  ,src= 'data/osm_extracts/greater-london-latest.osm.pbf')
  
  ,'oxford' =list('bbox'=c('west'=-1.3249
                           ,'east'=-1.1714
                           ,'south'=51.7079
                           ,'north'= 51.7948)
                  ,src= 'data/osm_extracts/england-latest.osm.pbf')
  
  ,'paris' = list('bbox'=c('west'=1.9569
                           ,'east'=2.8413
                           ,'south'=48.5684
                           ,'north'= 49.0784 )
                  ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'marseille' = list('bbox'=c('west'=5.2930
                               ,'east'=5.5426
                               ,'south'=43.2304
                               ,'north'= 43.3784)
                      ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'lyon' = list('bbox'=c('west'=4.7056
                          ,'east'=5.0427
                          ,'south'=45.6544
                          ,'north'= 45.8388)
                 ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'toulouse' = list('bbox'=c('west'=1.2710
                              ,'east'=1.6181
                              ,'south'=43.5102
                              ,'north'= 43.6805)
                     ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'bordeaux' = list('bbox'=c('west'=-0.7038
                              ,'east'=-0.4899
                              ,'south'=44.7501
                              ,'north'= 44.8943)
                     ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'lille' = list('bbox'=c('west'=2.9471
                           ,'east'=3.2616
                           ,'south'=50.5750
                           ,'north'= 50.7669)
                  ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'aix-en-provence' = list('bbox'=c('west'=5.3969
                                     ,'east'=5.4848
                                     ,'south'=43.5096
                                     ,'north'= 43.5517)
                            ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  
  ,'nice' = list('bbox'=c('west'=7.2369
                          ,'east'=7.3115
                          ,'south'=43.6849
                          ,'north'= 43.7337)
                 ,src = 'data/osm_extracts/france-latest.osm.pbf')
  
  ,'zurich' = list('bbox'=c('west'=8.4634
                            ,'east'=8.6126
                            ,'south'=47.3351
                            ,'north'= 47.4043)
                   ,src = 'data/osm_extracts/switzerland-latest.osm.pbf')
  
  ,'geneva' = list('bbox'=c('west'=6.0555
                            ,'east'=6.1963
                            ,'south'=46.1627
                            ,'north'= 46.2440)
                   ,src = 'data/osm_extracts/switzerland-latest.osm.pbf')
  
  ,'vienna' = list('bbox'=c('west'=16.2543
                            ,'east'=16.5276
                            ,'south'=48.1184
                            ,'north'= 48.2923)
                   ,src = 'data/osm_extracts/austria-latest.osm.pbf')
  
  ,'copenhagen' = list('bbox'=c('west'=12.4842
                                ,'east'=12.6580
                                ,'south'=55.6313
                                ,'north'= 55.7258)
                       ,src = 'data/osm_extracts/denmark-latest.osm.pbf')
  
  ,'barcelona' = list('bbox'=c('west'=2.0586
                               ,'east'=2.2618
                               ,'south'=41.3095
                               ,'north'= 41.4744)
                      ,src = 'data/osm_extracts/spain-latest.osm.pbf')
  
  ,'tbilisi' = list('bbox'=c('west'=44.6979
                             ,'east'=44.9416
                             ,'south'=41.6509
                             ,'north'= 41.8368)
                    ,src = 'data/osm_extracts/georgia-latest.osm.pbf')
  
  ,'santiago' = list('bbox'=c('west'=-70.8419
                              ,'east'=-70.4855
                              ,'south'=-33.6426
                              ,'north'= -33.3162)
                     ,src = 'data/osm_extracts/chile-latest.osm.pbf')
  
  ,'capetown' = list('bbox'=c('west'=18.3671
                              ,'east'=18.6239
                              ,'south'=-34.0271
                              ,'north'= -33.8419)
                     ,src = 'data/osm_extracts/south-africa-latest.osm.pbf')
  
  ,'montreal' = list('bbox'=c('west'=-73.9689
                              ,'east'=-73.3571
                              ,'south'=45.3502
                              ,'north'= 45.7158)
                     ,src = 'data/osm_extracts/canada-latest.osm.pbf')
  
  ,'mexico' = list('bbox'=c('west'=-99.2944
                            ,'east'=-98.9710
                            ,'south'=19.2716
                            ,'north'= 19.5553)
                   ,src = 'data/osm_extracts/mexico-latest.osm.pbf')
  
  ,'cairo' = list('bbox'=c('west'=31.1421
                           ,'east'=31.3550
                           ,'south'=29.9463
                           ,'north'= 30.1151)
                  ,src = 'data/osm_extracts/egypt-latest.osm.pbf')
  
  ,'rio-de-janeiro' = list('bbox'=c('west'= -43.4908
                                    ,'east'= -42.9689
                                    ,'south'= -23.0197
                                    ,'north'= -22.6869)
                           ,src = 'data/osm_extracts/sudeste-latest.osm.pbf')
  
  ,'seoul' = list('bbox'=c('west'= 126.6998
                           ,'east'= 127.1798
                           ,'south'= 37.4618
                           ,'north'= 37.7045)
                  ,src = 'data/osm_extracts/south-korea-latest.osm.pbf')
  
  ,'melbourne' = list('bbox'=c('west'= 144.6172
                               ,'east'= 145.2365
                               ,'south'= -38.0167
                               ,'north'= -37.6284)
                      ,src = 'data/osm_extracts/australia-latest.osm.pbf')
  
  ,'sydney' = list('bbox'=c('west'= 150.9995
                            ,'east'= 151.3140
                            ,'south'= -33.9883
                            ,'north'= -33.7261)
                   ,src = 'data/osm_extracts/australia-latest.osm.pbf')
  
  ,'tokyo' = list('bbox'=c('west'= 139.2517
                           ,'east'= 140.2954
                           ,'south'= 35.3017
                           ,'north'= 36.0136)
                  ,src = 'data/osm_extracts/japan-latest.osm.pbf')
  
  ,'moscow' = list('bbox'=c('west'= 37.3604
                            ,'east'= 37.8506
                            ,'south'= 55.5706
                            ,'north'= 55.9138)
                   ,src = 'data/osm_extracts/central-fed-district-latest.osm.pbf')
  ,'monaco' = list('bbox'=c('west'= 7.4068
                            ,'east'= 7.4446
                            ,'south'= 43.7237
                            ,'north'= 43.7537)
                   ,src = 'data/osm_extracts/monaco-latest.osm.pbf')
  
)

rlist::list.save(bbox_of_interest,"cities.rds")
