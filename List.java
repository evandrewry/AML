import java.util.*;

public class List extends LinkedList
{
    
    
    public List(Object [] arr)
    {
        super();
        for (int i = 0; i < arr.length; i++) add(arr[i]);
    }

}
