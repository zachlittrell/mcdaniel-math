import java.math.BigInteger;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.List;
import java.util.LinkedList;

public class Week5Problem2
{
  /**
   * A task that computes the longest valid subset for a given
   * range.
   */
  public static class IntervalTask
      implements Callable<List<Integer>>
  {
    private final BigInteger[] powersOfTwo;
    private final int max;
    private final int start;
    private final int end;
    /**
     *@param powersOfTwo  The necessary powers of two precomputed.
     *@param  max  The supremum of the superset
     *@param  start  The start of the interval
     *@param  step  The step between members of the interval
     */
    public IntervalTask(BigInteger[] powersOfTwo, int max, int start, int end)
    {
      this.powersOfTwo = powersOfTwo;
      this.max = max;
      this.start = start;
      this.end = end;
    }
    public List<Integer> call() throws Exception
    {
      final BigInteger MAX_SUBSET = 
	powersOfTwo[max].subtract(BigInteger.ONE);
      //Loop through how many bits we're going to turn off
      for (int i = start; i < end; i++)
      {
	ArrayDeque<BigInteger> maskStack = new ArrayDeque<BigInteger>();
	ArrayDeque<Integer> indices = new ArrayDeque<Integer>();
	ArrayDeque<Integer> counts = new ArrayDeque<Integer>();
	maskStack.push(MAX_SUBSET);
	indices.push(0);
	counts.push(0);
	do
	{
	  BigInteger mask = maskStack.pop();
	  int index = indices.pop();
	  int count = counts.pop();
          //Loop through each bit in mask
	  for (; index < max; index++)
	  {
            if (mask.testBit(index))
	    {
              BigInteger modifiedMask = mask.xor(powersOfTwo[index]);
	      int newCount = count+1;
	      if (newCount == i)
	      {
                List<Integer> subset = 
		  binaryNumberToSubset(max,modifiedMask);
		if (isValid(subset))
		  return subset;
	      }
              else
	      {	
		//Go to the next permutation
		//without flipping off the current bit
                maskStack.push(mask);
	        indices.push(index+1);
	        counts.push(count);

	        //Go to the next permutation
		//with flipping this bit off.
	        maskStack.push(modifiedMask);
	        indices.push(index+1);
		counts.push(newCount);
	      }
	    } 
      	  }
	} while (!maskStack.isEmpty());
      }
      //If we find no valid subset, we simply throw an exception
      //to signal that this thread has failed.
      throw new Exception();
    }
  }

  /**
   * Returns the subset of {1,2..max} represented by string
   */
  public static List<Integer> binaryNumberToSubset(int max,
                                                   BigInteger string)
  {
    ArrayList<Integer> list = new ArrayList<Integer>();
    for (int i = 0; i < max;)
    {
      if (string.testBit(i))
      {
        list.add(++i);
      }
      else
	i++;
    }
    return list;
  }

  /**
   * Returns true if n is a perfect square.
   * I eventually may replace this with a pure-integer
   * method so I don't rely on floating point math.
   */
  public static boolean isPerfectSquare(int n)
  {
    return n == Math.pow((int)Math.sqrt(n),2);
  }

  /**
   * Returns true if the product of any 3 distinct elements in set
   * is not a perfect square.
   */
  public static boolean isValid(List<Integer> set)
  {
    for (int x : set)
    {
      for (int y : set)
      {
	if (x != y)
	{
	  for (int z : set)
	  {
	    if (x != y && isPerfectSquare(x*y*z))
	      return false;
	  }
	}
      }
    }
    return true;
  }

  public static BigInteger[] powers(BigInteger n, int maxPower)
  {
    BigInteger[] powers = new BigInteger[maxPower+1];
    BigInteger ithPower = BigInteger.ONE;
    for (int i = 0; i <= maxPower; i++)
    {
      powers[i] = ithPower;
      ithPower = ithPower.multiply(n);
    }
    return powers;
  }

  /**
   * Returns the longest subset of {1,2..max} such that
   * the product of all three distinct integers in the subset
   * is not a perfect square.
   */
  public static List<Integer> compute(int max)
  {
    BigInteger[] powersOfTwo = powers(BigInteger.valueOf(2),max);
    int processors = Runtime.getRuntime().availableProcessors();
    //We'd like to use as many processors as we can to distribute
    //the task.
    ExecutorService threads = 
      Executors.newFixedThreadPool(processors,
	  new ThreadFactory(){
	    public Thread newThread(Runnable r){
	      Thread thread = 
	        Executors.defaultThreadFactory().newThread(r);
	      thread.setDaemon(true);
	      return thread;
	    }
	  });
    List<Future<List<Integer>>> futures =
      new ArrayList<Future<List<Integer>>>();
    final int step = (max+1)/processors;
    int start = 0;
    for (int i = 0; i < processors-1; i++)
    {
      int next = start + step;
      futures.add(threads.submit(new IntervalTask(powersOfTwo,max,start,next)));
      start = next+1;
    }
    futures.add(threads.submit(new IntervalTask(powersOfTwo,max,start,start+step+((max+1)%processors))));
    List<Integer> longest = null;
    for (Future<List<Integer>> future : futures)
    {
      try
      {
	longest = future.get();
	break;
      }
      catch(ExecutionException ee)
      {
      }
      catch(InterruptedException ie)
      {
	ie.printStackTrace();
      }
    }
    threads.shutdown();
    return longest;
  }

  public static void main(String[] args)
  {
    if (args.length == 0)
    {
      System.out.println("Remember to add the max value for the input set.");
      System.out.println(">java Week5Problem2 15");
    }
    else
    {
      int max = Integer.parseInt(args[0]);
      System.out.println(compute(max));
    }
  }
}
