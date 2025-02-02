import subprocess
liwc_path1 = "C:\\Program Files\\LIWC-22\\LIWC-22-cli.exe"
liwc_dict3 = "C:\\Users\\Al-Ahdal\\Downloads\\Spanish.dicx"
text_file3 = "C:\\Users\\Al-Ahdal\\Desktop\2019resultsnuts3_tweet_lang_es.csv"
outputLocation3 = "C:/Users/Al-Ahdal/Desktop/esLIWC2019results.csv"
cmd_to_execute = ["LIWC-22-cli",
                      "--mode", "wc",
                      "--input", text_file3,
                      "--row-id-indices", "1",
                      "--column-indices", "4",
                      "--output", outputLocation3]
subprocess.call(cmd_to_execute)






"C:\Users\Al-Ahdal\Downloads\LIWC2007 Dictionary - French.dicx"
"C:\Program Files\LIWC-22\LIWC-22-cli.exe"
"C:\Users\Al-Ahdal\Downloads\es2021.csv"
"C:\Users\Al-Ahdal\Downloads\LIWC2007 Dictionary - Spanish.dicx"