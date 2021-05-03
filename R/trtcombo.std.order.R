
#' @title A Function to generate the 2^n treatment combinations in their standard order,
#' for a symmetric 2^n factorial experiment.
#'
#' @description The function basically utilizes a very basic algorithm, for listing down
#' all possible treatment combinations in a 2^n experiment, where stating the treatment combinations
#' for large values of n, say n = 8, i.e., a 2^8 experiment, could be quite a task, as there are a total
#' of 2^8 = 256 treatment combinations to be listed, starting from 1,a,b,ab,......,abcdefgh.
#'
#' Hence, the function eases the task of listing down all the possible treatment combinations, in a specific
#' standard order, irrespective of the magnitude of 'n'.
#'
#' For example - Suppose we consider a 2^4 factorial experiment. The total number of treatment combinations
#' are 2^4 = 16. Implementing the function would list down all possible treatment combinations in a standard order
#' as follows, '1','a','b','ab','c','ac','bc','abc','d','ad','bd','abd','cd','acd','bcd','abcd'.
#'
#' Therefore, the algorithm of listing down the Treatment Combinations in a 2^n Factorial Experiment, in the
#' standard order is as follows,
#'
#' Step 1 :: Start with the control group labeled as '1', where all factors considered in the design are at a
#'           lower level.
#'
#' Step 2 :: Select a new alphabet (denoting the respective factors) in lexicographic order, multiplying (concatenating)
#'           it with the previous alphabets till the last in the list.
#'
#' Step 3 :: Repeat Step 2 until all the alphabets, representing specific factors in the experiment, gets exhausted.
#'
#' @author Somjit Roy
#'
#' @param n A numeric value - Denoting the number of factors involved in a 2^n factorial experiment.
#'
#' @return A character type vector comprising of all possible treatment combinations in a 2^n factorial experiment
#' in their standard order.
#'
#' @export
#'
#' @seealso The generation of all possible treatment combinations in a specific standard order in the symmetric 2^n
#' factorial experiment, is of great importance because it helps to record all the combinations without missing out
#' on any of the combinations, especially when 'n', i.e., the number of factors involved in the experiment is quite
#' large.
#'
#' Listing down the treatment combinations in their standard order, gets utilized in the analysis of the factorial
#' experiment under consideration, also this way of reporting and listing the treatment combinations facilitates
#' during 'construction of Fractional Factorial Experiment', 'Confounding of treatment effects in a factorial experiment',
#' etc.
#'
#'
#' @examples
#' # For a 2^2 factorial experiment, we will obtain all the 4 possible treatment combinations,
#' # in their standard order as , '1','a','b' and 'ab'. Running the following line of code
#' # would give the desired result.
#'
#' trtcombo.std.order(2)
#'
#' # Similarly, for a 2^5 experiment, we will obtain all the 32 possible
#' # treatment combinations, in their standard order, by running the following line of code,
#'
#' trtcombo.std.order(5)

trtcombo.std.order = function(n) # A Function generating the treatment combinations
                                 # of a 2^n Factorial Experiment in the Standard
                                 # Order.
{
  fact = letters[1:n] # Generating the labels of the 'n' factors(treatments)
                      # in a (2^n) Factorial Experiment.

  control = '1'       # Denotes the Control Group ---> All Factors at lower level
                      # or absent.

  trt.combo = array(dim=1) # An Array storing all possible treatment
                           # combinations in a (2^n) Factorial Experiment, in
                           # Standard Order.

  ini.1 = array(dim=1)   # A Dummy Array to store required treatment combinations.

  trt.combo[1] = control # Storing the control group as the first element in the
                         # treatment combination.

  # Performing the Algorithm to generate the required treatment combinations
  # in a 2^n Factorial Experiment, in their respective Standard Order.
  for(i in 1:length(fact))
  {
    v = fact[i]
    for(j in 1:length(trt.combo))
    {
      if(trt.combo[j] == '1')
      {
        ini.1[j] = paste(v)
      }else
      {
        ini.1[j] = paste(trt.combo[j],v) # Using the paste() function to multiply
                                         # the corresponding letters, denoting a
                                         # particular treatment combination.
      }

    }
    trt.combo = c(trt.combo,ini.1)
  }
  return(trt.combo)
}
