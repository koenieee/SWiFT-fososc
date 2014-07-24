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
import net.lingala.zip4j.core._
import net.lingala.zip4j.model._
import net.lingala.zip4j.util._

class AdminPage
{  // the SessionVar will contain a String with "Anonymous" as default value.
   object JaraDur extends SessionVar[String]("1")
   
   log("AdminPage called")

   def settings =
   {  "#jsonscript"             #> Script(json.jsCmd) &
     ".startSimu [onclick]"     #> Text(json.call(ElemById("startSimu") ~> Value,ElemById("inputbox") ~> Value).toJsCmd) &
     ".downloadInfo [onclick]"  #> Text(json.call(ElemById("downloadInfo") ~> Value,ElemById("passwd") ~> Value).toJsCmd)
   }

  def zip4j(pwd: String):Unit=
  { log("creating zip4j")
    log("deleting previous file")
    new java.io.File("src/main/webapp/output.zip").delete()

    val zipFile: ZipFile  = new ZipFile("src/main/webapp/output.zip");

    val parameters: ZipParameters  = new ZipParameters();

    parameters.setCompressionMethod(Zip4jConstants.COMP_DEFLATE);
    parameters.setCompressionLevel(Zip4jConstants.DEFLATE_LEVEL_NORMAL);

    if(!pwd.isEmpty)
    { parameters.setEncryptFiles(true);
      parameters.setEncryptionMethod(Zip4jConstants.ENC_METHOD_STANDARD);
      parameters.setPassword(pwd);
    }
    val consti: String = "src/main/webapp/constitutions/";
    val persists: String = "persist/";
    val swiftVersion: File = new java.io.File("persistentDatastructureMainVersion.txt")
    val liftDB: File = new java.io.File("lift_proto.db.h2.db")


    zipFile.addFolder(consti, parameters);
    zipFile.addFolder(persists, parameters);
    zipFile.addFile(swiftVersion, parameters);
    zipFile.addFile(liftDB, parameters);


  }

  object json extends JsonHandler
  { def apply(in: Any): JsCmd =
    { in match
      { case JsonCmd("submit", _, p: String, _) =>
        { SetHtml("jararesult",Text("Running Simulation.."));
          JaraDur.set(p)
          GlobalConstant.clearAndReinitialiseSWiFTdatabase

          log("Calling Jara")
          PlayingSimulator.start(JaraDur.is.toLong * 1000 * 60 * 60)

          SetHtml("jararesult", Text("Simulation Ended!"))
        }
        case JsonCmd("download", _, p: String, _) =>
        { log("zip password is: " + p)
          zip4j(p)
          log("zip file done")

          SetHtml("jararesult",Text("Ready for download<br>Your zip password is: " + p));

          S.redirectTo("/output.zip")
        }
      }
    }
  }

  
		
		

		
}
}


