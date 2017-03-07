if (nchar(Sys.getenv("SPARK_HOME"))<1) {
  Sys.setenv(SPARK_HOME = "C:\\Spark\\spark-2.1")
}

#Sys.setenv(JAVA_HOME= "C:/Program Files/java/jre1.8.0_121/")


library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"),"R", "lib")))

#sc <- sparkR.session(master = "local")

sc <- sparkR.session(master = "local[*]", sparkEnvir = list(spark.driver.memory = "1.5g"))
sqlContext <- sparkR.session(sc)

music <- read.df("/E:/capstone_project/Musical_Instruments_5.json", "json", inferschema = TRUE)
printSchema(music)
head(music)
count(music)


#library(rJava)


