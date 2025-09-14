test_that("connetions works", {
conn <-   init.conn(
     db.credentials = list(
         BIAD_DB_USER = "test_user",
         BIAD_DB_PASS = "test1234",
         BIAD_DB_HOST = "127.0.0.1",
         BIAD_DB_PORT = 3306
     )
  )
 expect_type(conn,"S4")
 expect_s4_class(conn,"MySQLConnection")
})

test_that("disconnection works", {
    res <- suppressWarnings(disconnect())
    testthat::expect_length(res,1)
    testthat::expect_true(res)
    res <- disconnect()
    testthat::expect_length(res,0)
})

test_that("get.relative works", {
    conn <-   init.conn(
         db.credentials = list(
             BIAD_DB_USER = "test_user",
             BIAD_DB_PASS = "test1234",
             BIAD_DB_HOST = "127.0.0.1",
             BIAD_DB_PORT = 3306
         )
    )
    res <- get.relatives(table.name="Sites",primary.value="S12592")
    testthat::expect_type(res,'list')
    testthat::expect_length(res,1)
    testthat::expect_named(res,"S12592")
    testthat::expect_named(res[[1]],c("data","up","down"))
    testthat::expect_true(any(grepl("FRUNT1",names(unlist(res,recursive=T)))))
})
