
/********************** CLASS BINARY **********************
This class represents a bit word. It is used to represent the int value of every bit word and its number of ones.
**********************************/
public class Binary {
	
	private int value;
	private int numOfOnes;
	
	public Binary(int v)
	{
		value=v;
	}
	public Binary(int v, int n)
	{
		value=v;
		numOfOnes=n;
	}
	public void setValue(int v)
	{
		value=v;
	}
	public void setNumOfOnes(int n)
	{
		numOfOnes=n;
	}
	public int getValue()
	{
		return value;
	}
	public int getNumOfOnes()
	{
		return numOfOnes;
	}
	public boolean equals(Object x)
	{
		boolean result=false;
		Binary b=(Binary)x;
		if(this.value==b.value)
			result=true;
		return result;
	}
	public String toString()
	{
		String result;
		result=value+" "+numOfOnes;
		return result;
	}

}
