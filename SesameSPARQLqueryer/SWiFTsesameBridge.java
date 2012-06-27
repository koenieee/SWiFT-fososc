/* 
## Compile command (change path to appropriate ones for your installation)

javac -classpath .;C:\Sesame\openrdf-sesame-2.1.3\lib\openrdf-sesame-2.1.3-onejar.jar SWiFTsesameBridge.java

from any directory: 

javac -classpath D:\move2subleme\sesameLearning\SWiFTbridge;C:\Sesame\openrdf-sesame-2.1.3\lib\openrdf-sesame-2.1.3-onejar.jar D:\move2subleme\sesameLearning\SWiFTbridge\SWiFTsesameBridge.java

## Interpreter command

java -classpath .;C:\Sesame\openrdf-sesame-2.1.3\lib\openrdf-sesame-2.1.3-onejar.jar;C:\Sesame\openrdf-sesame-2.1.3\lib\slf4j-api-1.5.0.jar;SWiFTsesameBridge;C:\Sesame\openrdf-sesame-2.1.3\lib\slf4j-jdk14-1.5.0.jar SWiFTsesameBridge

from any directory:

java -classpath D:\move2subleme\sesameLearning\SWiFTbridge;C:\Sesame\openrdf-sesame-2.1.3\lib\openrdf-sesame-2.1.3-onejar.jar;C:\Sesame\openrdf-sesame-2.1.3\lib\slf4j-api-1.5.0.jar;SWiFTsesameBridge;C:\Sesame\openrdf-sesame-2.1.3\lib\slf4j-jdk14-1.5.0.jar D:\move2subleme\sesameLearning\SWiFTbridge\SWiFTsesameBridge


*/

// Java standard API
import java.io.*;
import java.util.*;

// Sesame API
import org.openrdf.repository.*;
import org.openrdf.rio.*;
import org.openrdf.model.*;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;
import org.openrdf.OpenRDFException;
import java.util.List;
import org.openrdf.OpenRDFException;
import org.openrdf.query.*;
/*
import org.openrdf.query.TupleQuery;
import org.openrdf.query.TupleQueryResult;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryLanguage;
*/ 
public class SWiFTsesameBridge
{  public static void main(String[] args)
   {  // Stage 1: open files with fixed file names (translation.ttl) to read turtle expressions and SPARQL query (query.txt), further files: queryResult.txt, queryErrors.txt, turtleErrors.txt
      String query = "";
      try
      {  BufferedReader inputStream = new BufferedReader(new FileReader("query.txt"));
         String line = "";
      
         while( (line = inputStream.readLine()) != null)
         {  query = query + line; 
         }
         System.out.println("Query: " + query);
      }
      catch (Exception e)
      {  System.out.println("Exception: " + e.getMessage());
      }
      
      PrintWriter queryResultStream = null;
      try
      {  queryResultStream = new PrintWriter(new BufferedWriter(new FileWriter("queryResult.txt")));
      }
      catch (Exception e)
      {  System.out.println("Exception: " + e.getMessage());
      }      
      
      queryResultStream.println("Bridge from SWiFT Subleme to SPARQL engine of Sesame 2.1. Chide Groenouwe V200903101147");
      queryResultStream.println("Query: " + query);
      queryResultStream.println("Results:");
      // Stage 2: load repository (put result in file with name loadreposresult.txt, if succesful, ) and execute query  (put result in queryresult.txt)
      // TODO: better move this to the part where the file is actually being written, so that this while will not be created when something goes wrong (exceptions.)
      try
      {  Repository myRepository = new SailRepository(new MemoryStore());
         try
         {  myRepository.initialize();
         }
         catch (Exception e)
         {  System.out.println("Exception: " + e.getMessage());
         }

         File file = new File("translation.ttl");   
         String baseURI = "http://thisdocument/";
         
         try 
         {  RepositoryConnection con = myRepository.getConnection();
            try 
            {  con.add(file, baseURI, RDFFormat.TURTLE); 

               try 
               {  //String queryString = "SELECT ?s ?p ?o  WHERE {?s ?p ?o.}";
                  TupleQuery tupleQuery = con.prepareTupleQuery(QueryLanguage.SPARQL, query);
                  TupleQueryResult result = tupleQuery.evaluate();
                  try 
                  {  int n = 0;
                     
                     if(!result.hasNext())
                     {  queryResultStream.print("Result set is empty");}
                     while (result.hasNext()) 
                     {  n++;
                        queryResultStream.println("Result " + n + ":");
                        BindingSet bindingSet = result.next();
                        Iterator<Binding> bindingIt = bindingSet.iterator();
                        Binding b;
                        
                        while (bindingIt.hasNext())
                        {  b    = bindingIt.next();
                           queryResultStream.println( "   " + b.getName() + " = " + b.getValue(  ));
                        }                        
                     }
                  }
                  finally 
                  { result.close(); }
               }
               catch(Exception e)
               {  //System.out.println("Exception (L200903101345): " + e.getMessage());
                  queryResultStream.println("Error in query formulation: " + e.getMessage());
               }         
            }            
            finally
            {  con.close();
               
            }
         }            
         catch (OpenRDFException e) 
         {  //System.out.println("Exception (L200903101356): " + e.getMessage());
            queryResultStream.println("Error in turtle: " + e.getMessage());
         }
         catch (java.io.IOException e) 
         {  System.out.println("Exception: " + e.getMessage());
         }
      }
      catch (Exception e)
      {  System.out.println("Exception: " + e.getMessage());  
      }
      queryResultStream.close();
   }
}