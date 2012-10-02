import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
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
    private final int max;
    private final int start;
    private final int step;
    /**
     *@param  max  The supremum of the superset
     *@param  start  The start of the interval
     *@param  step  The step between members of the interval
     */
    public IntervalTask(int max, int start, int step)
    {
      this.max = max;
      this.start = start;
      this.step = step;
    }
    public List<Integer> call() throws Exception
    {
      BigInteger end = BigInteger.valueOf(2).pow(max);
      BigInteger step = BigInteger.valueOf(this.step);
      List<Integer> longest = Collections.<Integer>emptyList();
      for (BigInteger i = BigInteger.valueOf(start);
	   i.compareTo(end) <= 0;
	   i = i.add(step))
      {
        List<Integer> subset = binaryNumberToSubset(max,i);
	if (isValid(subset) && subset.size() > longest.size())
	  longest = subset;
      }
      return longest;
    }
  }

  /**
   * Returns the subset of {1,2..max} represented by string
   */
  public static List<Integer> binaryNumberToSubset(int max,
                                                   BigInteger string)
  {
    ArrayList<Integer> list = new ArrayList<Integer>();
    for (int i = 0; i < max; i++)
    {
      if (string.testBit(i))
      {
        list.add(i);
      }
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

  /**
   * Returns the longest subset of {1,2..max} such that
   * the product of all three distinct integers in the subset
   * is not a perfect square.
   */
  public static List<Integer> compute(int max)
  {
    int processors = Runtime.getRuntime().availableProcessors();
    //We'd like to use as many processors as we can to distribute
    //the task.
    ExecutorService threads = Executors.newFixedThreadPool(processors); 
    CompletionService<List<Integer>> pool =
      new ExecutorCompletionService<List<Integer>>(threads);
    for (int i = 0; i < processors; i++)
      pool.submit(new IntervalTask(max,i,processors));
    List<Integer> longest = Collections.<Integer>emptyList();
    for (int i = 0; i < processors; i++)
    {
      try
      {
        List<Integer> subset = pool.take().get();
        if (subset.size() > longest.size())
       	  longest = subset;
      }
      catch(Exception ex)
      {
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
