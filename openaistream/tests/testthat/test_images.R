test_that("images",{
  skip_on_cran()
  library(png)
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  pic1<-handle_openai$images$create(prompt = "A small bird flies over the ocean, heading towards the storm.")
  test_dir = tempdir()
  expect_contains(object = names(pic1),expected = "data")
  pic_file1<-paste0(test_dir,"/","pic1.png")
  req <- request(pic1$data$url) %>%req_perform(path = pic_file1)
  expect_equal(req$status_code,200)
  expect_true(file.exists(pic_file1))
  #Here is the image test.
  #For simple test,you need install 'png' package.
  #Note that when a mask is not provided, OpenAI defaults to using areas of the uploaded image with a transparency of 0 as the modification area.
  #The following test adds full image transparency of 0 (modifying the entire image) to pic1.
  pic1<-readPNG(pic_file1)
  expect_type(pic1,"double")
  expect_equal(dim(pic1)[3],3)

  alpha_channel <- matrix(0, nrow = dim(pic1)[1], ncol = dim(pic1)[2])
  pic1_b<-array(c(pic1,alpha_channel), c(1024,1024,4))
  expect_equal(dim(pic1_b)[3],4)
  pic_file2<-paste0(test_dir,"/","pic2.png")
  writePNG(pic1_b,target = pic_file2)
  expect_true(file.exists(pic_file2))
  pic3<-handle_openai$images$edit(image = pic_file2,prompt = "Please modify the bird in the picture to a flock of birds."
                         ,verbosity = 3,n="2",size="256x256")
  expect_contains(object = names(pic3),expected = "data")
  expect_equal(length(pic3$data$url),2)
  pic_file3_1<-paste0(test_dir,"/","pic3_1.png")
  pic_file3_2<-paste0(test_dir,"/","pic3_2.png")
  request(pic3$data$url[1]) %>% req_perform(path = pic_file3_1)
  request(pic3$data$url[2]) %>% req_perform(path = pic_file3_2)
  expect_true(file.exists(pic_file3_1))
  expect_true(file.exists(pic_file3_2))

  pic4<-handle_openai$images$variation(image = pic_file1,n="2",size="256x256")
  expect_contains(object = names(pic4),expected = "data")
  expect_equal(length(pic4$data$url),2)
  pic_file4_1<-paste0(test_dir,"/","pic4_1.png")
  pic_file4_2<-paste0(test_dir,"/","pic4_2.png")
  request(pic4$data$url[1]) %>%req_perform(path = pic_file4_1)
  request(pic4$data$url[2]) %>%req_perform(path = pic_file4_2)
  expect_true(file.exists(pic_file4_1))
  expect_true(file.exists(pic_file4_2))

  ##error test
  pic1<-handle_openai$images$create(prompt = "A small bird flies over the ocean, heading towards the storm.",verbosity = 4)
  expect_true(!pic1$success)

  pic3<-handle_openai$images$edit(image = NULL,prompt = "Please modify the bird in the picture to a flock of birds."
                                   ,n="2",size="256x256")
  expect_true(!pic3$success)
  pic3<-handle_openai$images$edit(image = pic_file2,mask="/tmp/sadasdasd",prompt = "Please modify the bird in the picture to a flock of birds."
                                   ,n="2",size="256x256")
  expect_true(!pic3$success)
  pic3<-handle_openai$images$edit(image = pic_file2,mask=pic_file2,prompt = "Please modify the bird in the picture to a flock of birds."
                                   ,n="2",size="256x256")
  expect_contains(object = names(pic3),expected = "data")
  expect_equal(length(pic3$data$url),2)
  pic3<-handle_openai$images$edit(image = pic_file2,mask=pic_file1,prompt = "Please modify the bird in the picture to a flock of birds."
                                   ,n="2",size="256x256")
  expect_true(!pic3$success)
  pic4<-handle_openai$images$variation(image = NULL,n="2",size="256x256")
  expect_true(!pic4$success)
  pic4<-handle_openai$images$variation(image = pic_file1,n="2",size="256x256",verbosity = 4)
  expect_true(!pic4$success)
})
