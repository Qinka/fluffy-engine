c <- connectPostgreSQL "host=route.etvp.qinka.pro port=54321 dbname=spm user=qinka password=johnjing"

Right (Pandoc _ bs) <- loadFileWithDocx "/Users/Qinka/Downloads/2017spm/XDExamReview2017/TrueFalse/Chapter_1_Introduction_to_Project_Management.docx"
let tofs_b = drop 1 $ take 32 bs
    tofs   = map toTOF tofs_b
mapM_ (updateTOFs c) tofs

c <- PG.connectPostgreSQL "host=etvp-400 port=54321 dbname=spm"

Right (Pandoc _ bs) <- loadFileWithDocx "/Users/Qinka/Downloads/2017spm/XDExamReview2017/MultipleChoice/Time.docx"
let a = map parseMCfBlock bs
    b = toMCPfMCC a
    d = map toMCfMCP b
mapM_ (updateMC c) d
