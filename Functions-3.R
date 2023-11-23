Preallocated_URLdecode = 
  function (URL) 
  {
    vapply(URL, function(URL) {
      x <- charToRaw(URL)
      pc <- charToRaw("%")
      out <- raw(length = length(URL))
      i <- 1L
      j <- 1L
      while (i <= length(x) && j <= length(x)) {
        if (x[i] != pc) {
          out[j] <- x[i]
          i <- i + 1L
          j <- j + 1L
        }
        else {
          y <- as.integer(x[i + 1L:2L])
          y[y > 96L] <- y[y > 96L] - 32L
          y[y > 57L] <- y[y > 57L] - 7L
          y <- sum((y - 48L) * c(16L, 1L))
          out[j] <- as.raw(as.character(y))
          i <- i + 3L
          j <- j + 1L
        }
      }
      rawToChar(out)
    }, character(1), USE.NAMES = FALSE)
  }

Vectorization_URLDecode = 
  function(Str)
  {
    x = charToRaw(Str)
    pc = charToRaw("%")
    PercentageIndex = which(x == pc)
    s = strsplit(Str, "")[[1]]
    
    DeletionVec = rep(PercentageIndex, each = 2) + c(1,2)
    ExtractResult = x[DeletionVec]
    y = as.integer(ExtractResult)
    y[y > 96L] <- y[y > 96L] - 32L
    y[y > 57L] <- y[y > 57L] - 7L
    matrixY = matrix(y, ncol = 2, byrow = TRUE)
    matrixY = matrixY - 48L
    matrixY[,1] = matrixY[,1] * 16L
    ConversionVec = rowSums(matrixY)
    ConversionVec = as.raw(as.character(ConversionVec))
    ConversionVec = rawToChar(ConversionVec)
    ConversionVec = strsplit(ConversionVec, "")[[1]]
    s[PercentageIndex] = ConversionVec
    
    Result = s[-DeletionVec]
    Result = paste(Result, collapse = "")
    
    return(Result)
  }

OriginalRunningTime = 
  function(Str)
  {
    start_origin = Sys.time()
    DecodeResult = utils::URLdecode(Str)
    end_origin = Sys.time()
    
    running_time = end_origin - start_origin
    return(as.numeric(running_time))
  }

PreallocationRunningTime =
  function(Str)
  {
    start_preallocation = Sys.time()
    Preallocation_DecodeResult = Preallocated_URLdecode(Str)
    end_preallocation = Sys.time()
    
    running_time = end_preallocation - start_preallocation
    return(as.numeric(running_time))
  }

VectorizationRunningTime =
  function(Str)
  {
    start_vectorization = Sys.time()
    Vectorization_DecodeResult = Vectorization_URLDecode(Str)
    end_vectorization = Sys.time()
    
    running_time = end_vectorization - start_vectorization
    return(as.numeric(running_time))
  }    