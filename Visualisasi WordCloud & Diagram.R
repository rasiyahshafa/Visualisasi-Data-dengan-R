#Library
library(dplyr) #digunakan untuk analisis dan eksplorasi data
library(tidyr) #digunakan untuk import data file
library(NLP) #digunakan untuk preprocessing data
library(tm) #digunakan untuk cleansing data
library(stringr) #digunakan untuk integrasi karakter
library(caret) #digunakan untuk menguji berbagai prosedur pemodelan
library(ggplot2) #digunakan untuk membuat berbagai bentuk grafik
library(RColorBrewer) #digunakan untuk memberikan pewarnaan pada visualisasi
library(wordcloud) #pembuatan wordcloud
library(tokenizers) #proses number removal, puntuation removal (tanda baca)

#Read file 
data1 <- read.csv(file.choose(), header = T)
View(data1)

#Build corpus
opini <- iconv(data1$Menurut.kamu.bagaimana.seharusnya.pembelajaran.jarak.jauh.yang.efektif., to = "utf-8")
opini <- Corpus(VectorSource(opini))
inspect(opini[1:30])

#Clean text
cleantext <- tm_map(opini, tolower)
inspect(cleantext[1:30])

cleantext <- tm_map(cleantext, removePunctuation)
inspect(cleantext[1:30])

stop_word <- readLines("C:/Stopword/stopword.csv")
data_stopword <- tm_map(cleantext, removeWords, stop_word)
inspect(data_stopword[1:30])

#Menghapus kata-kata tidak penting
tambahan_stopword <- tm_map(data_stopword, removeWords,
                            c('dengan','metode','project','ini','diprakarsai','hasil','implikasi','surat','edaran','mendikbud',
                              'no4','2020','project','memiliki','tujuan','utama','pelatihan','pelajar','berkolaborasi','gotong',
                              'royong','empati','mendikbud','efektif','diterapkan','pelajar','membentuk','projek',
                              'pembelajaran','cocok','zona','kuning','hijau','menjalankan','memperhatikan','protokol','kesehatan',
                              'ketat','melakukan','idk','serap','namanya','jarak','yaa','yg',
                              'selayaknya','duduk','diam','menerus','bikin','cerna',
                              'alangkah','baiknya','bahan','contoh','ppt','jelaskansebelumnya','mengalami','gangguan',
                              'sinyal','susah','mengakibatkan','pemaparan','lakukan','terhambat','menunda',
                              'nunda','menambah','beban','kalanya','sih','2x','gtu','mukaoffline','intens','3','7','jam',
                              '8','pagi','15','sore','selebihnya','biar','mengatur','dewasa','anak','muda',
                              'indonesia','dimedia','sosialnya','pembantu','berlaku','membosankan',
                              'menerapkan','mengaturnya','task','melalukan','nya','tugas','monoton','tubuh','mhs',
                              'dibebani','sehat','mhs','maya','dgn','tepatnya','sm2','dr','belah',
                              'mahasiswanya','vidio','ppt','covid19','kalo','diperbaiki','nya','apresiasi','murid','seminggu'))
inspect(tambahan_stopword[1:30])

docs <- tm_map(tambahan_stopword, gsub, pattern = "gampang", replacement = "mudah")
docs1 <- tm_map(docs, gsub, pattern = "offline", replacement = "tatapmuka")
docs2 <- tm_map(docs1, gsub, pattern = "sekreatif", replacement = "kreatif")
docs3 <- tm_map(docs2, gsub, pattern = "soal2", replacement = "latihan")
docs4 <- tm_map(docs3, gsub, pattern = "tool", replacement = "sarana")
docs5 <- tm_map(docs4, gsub, pattern = "dijaringan", replacement = "jaringan")
docs6 <- tm_map(docs4, gsub, pattern = "mahasiswai", replacement = "mahasiswa")
docs7 <- tm_map(docs4, gsub, pattern = "mahasiswasiswa", replacement = "mahasiswa")
docs8 <- tm_map(docs4, gsub, pattern = "pelajarmahasiswa", replacement = "mahasiswa")
inspect(docs8[1:30])

cleanset <- tm_map(docs5, stripWhitespace)
inspect(cleanset[1:30])

#Menyimpan data bersih
databersih <- data.frame(text=unlist(sapply(cleanset, '[')),stringsAsFactors = F)
write.csv(databersih, file ="C:/Stopword/databersih7.csv")

databersih7 <- read.csv(file.choose(), header = TRUE)
databersih7

corpustext <- Corpus(VectorSource(databersih7$text))
inspect(corpustext[1:30])

text = cleanset

strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(text)

tdm <- TermDocumentMatrix(corpustext)


t <- removeSparseTerms(tdm, sparse = 0.95)

#Pembuatan matriks kata
library(factoextra)
m <- as.matrix(t)

freq <- rowSums(m)
freq

barplot(freq, las=2, col = rainbow(10), cex.names = 0.7)
barplot

#Pembuatan wordcloud
library(wordcloud)
text <- as.character(text)
wordcloud(text, max.words = 15, random.color = TRUE,min.freq = 25,
          rot.per = 0.4,random.order = FALSE, colors = brewer.pal(5, "Dark2"),scale = c(5,0.4))

library(wordcloud2)
freq<- data.frame(names(freq), freq)
colnames(freq) <- c('text', 'freq')
wordcloud2(freq, size = 0.5, shape = 'star', minSize = 1)


