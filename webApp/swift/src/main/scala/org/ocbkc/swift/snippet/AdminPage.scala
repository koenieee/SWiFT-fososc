package org.ocbkc.swift 

package snippet 
{
import _root_.scala.xml._
import _root_.net.liftweb.util._
import Helpers._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global._
import ocbkc.swift.test.simulation.jara._
import java.io.File
import _root_.net.liftweb.http._
import js._
import JsCmds._
import JE._
/*
 * Cleanup non-used imports.
 * */
class AdminPage
{  // the SessionVar will contain a String with "Anonymous" as default value.
   object JaraDur extends SessionVar[String]("1")
   
   log("AdminPage called")
   
   //val path = GlobalConstant.WEBAPROOT
   //JsonCmd(2,null,false,Map(command -> 2, params -> false))

   var n_blaat = JaraDur.is 
   // println(lines)
	 
		
   def settings =
   {  "#jsonscript" #> Script(json.jsCmd) &
     ".startSimu [onclick]" #> Text(json.call(ElemById("startSimu") ~> Value,ElemById("inputbox") ~> Value).toJsCmd) &
     ".downloadInfo [onclick]" #> Text(json.call(ElemById("downloadInfo") ~> Value).toJsCmd)
   }


  def downloadPerInfo():Unit=
  { /*
    Files to compress:

    persistentDatastructureMainVersion.txt
    persist/
    src/main/webapp/constitutions/
    lift_proto.db.h2.db

     */
    log("DownloadInfo Called")
    val fileNames = List(
      "persistentDatastructureMainVersion.txt",
      "lift_proto.db.h2.db"
    ) ::: listFiles("persist/sessionInfoobjs") ::: listFiles("persist/constobjs") ::: listFiles("src/main/webapp/constitutions/").filterNot(file => file.contains(".git") == true)

    log("Ready for zipping: " + fileNames)
    zip("src/main/webapp/output.zip", fileNames)

    log("Zip file done")
    S.redirectTo("/output.zip")


  }

  def listFiles(dirName: String): List[String] =
  {
    new java.io.File(dirName).listFiles.map(_.toString).toList
  }

	
   object json extends JsonHandler 
   {  def apply(in: Any): JsCmd =
      {  in match 
         {  case JsonCmd("submit", _, p: String, _) => 
            { SetHtml("jararesult",Text("Running Simulation.."));
              JaraDur.set(p)
              GlobalConstant.clearAndReinitialiseSWiFTdatabase

              log("Calling Jara")
              PlayingSimulator.start(JaraDur.is.toLong * 1000 * 60 * 60)

              SetHtml("jararesult", Text("Simulation Ended!"))
            }
           case JsonCmd("download", _, _, _) =>
           {
             downloadPerInfo();
             log("Button is working")
             SetHtml("jararesult",Text("Ready for download"));
           }
      }
      }
   }

  def zip(out: String, files: List[String]) = {
    import java.io.{ BufferedInputStream, FileInputStream, FileOutputStream }
    import java.util.zip.{ ZipEntry, ZipOutputStream }

    val zip = new ZipOutputStream(new FileOutputStream(out))

    files.foreach { name =>
      zip.putNextEntry(new ZipEntry(name))
      val in = new BufferedInputStream(new FileInputStream(name))
      var b = in.read()
      while (b > -1) {
        zip.write(b)
        b = in.read()
      }
      in.close()
      zip.closeEntry()
    }
    zip.close()
  }
  
  
		
		

		
}
}


