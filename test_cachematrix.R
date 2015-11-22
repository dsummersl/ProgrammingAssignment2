library(testthat)

source("cachematrix.R")

context('cachematrix')

test_that("makeCacheMatrix() creates a list of functions", {
    m <- makeCacheMatrix()
    expect_equal(typeof(m),'list')
})

test_that("makeCacheMatrix().get returns a matrix", {
    m <- makeCacheMatrix()
    expect_identical(m$get(),matrix())
})

test_that("makeCacheMatrix(x) and .get returns a matrix", {
    m <- makeCacheMatrix(matrix(2,1,2))
    expect_identical(m$get(),matrix(2,1,2))
})

test_that("makeCacheMatrix()$set() updates the matrix", {
    m <- makeCacheMatrix()
    m$set(matrix(2,1,2))
    expect_identical(m$get(),matrix(2,1,2))
})

test_that("cacheSolve() returns an inverse, and caches it", {
    m <- makeCacheMatrix(matrix(data=c(1,0,0,1),2,2))
    expect_identical(cacheSolve(m),matrix(data=c(1,0,0,1),2,2))
    # expect_identical(m$getinverse(),matrix(data=c(1,0,0,1),2,2))
})

test_that("When makeCacheMatrix()$set() is called getinverse() is reset", {
    m <- makeCacheMatrix()
    m$setinverse(matrix(2,1,2))
    expect_identical(m$getinverse(),matrix(2,1,2))
    m$set(matrix(2,1,2))
    expect_identical(m$getinverse(),NULL)
    m$setinverse(matrix(2,1,2))
    expect_identical(m$getinverse(),matrix(2,1,2))
})
