## 计算矩阵的逆矩阵
## 但为了避免重复计算，会将计算所得的结果存入缓存
## 在计算新的矩阵的时候，会先查看缓存中是否已有结果
## 如果有结果了则直接取缓存中的结果；如果没有才进行计算，并将结果存入缓存

## 创建指定矩阵的逆矩阵的缓存
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}


## 和上一函数配合。
## 如果指定矩阵的逆矩阵已经计算，则直接从缓存中获取并返回；
## 如果没有计算，则进行计算并存储到缓存中
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
