package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import System._
import _root_.org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.OCBKC._
import org.ocbkc.swift.OCBKC.ConstiSelectionProcedure._
import org.eclipse.jgit.api._
import java.io._
import org.ocbkc.swift.snippet.sesCoord

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
   println("Boot.boot called")
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("org.ocbkc.swift")
    Schemifier.schemify(true, Schemifier.infoF _, Player)

    // Build SiteMap
    /* originally generated code:
    def sitemap() = SiteMap(
      Menu("Home") / "index" >> User.AddUserMenusAfter, // Simple menu form
      // Menu with special Link
      Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	       "Static Content")))

     */

/* TODO in row to be removed, this approach turns out not to work. Perhaps git stash it somewhere.
   def studyConstitutionLink = 
   {  println("studyConstitutionLink called")
      "studyConstitution?id=" + 
      {  Player.currentUser match 
         {  case Full(player) =>
               player.firstChosenConstitution match
               {  case Some(const) => const.id.toString
                  case _           => println("   BUG: no first chosen constition found"); "BugNoFirstChosenConstitutionFound"
               }
            case _              => println("   BUG: no player found"); "BugNoPlayerFound" // or is this no bug? Perhaps lift always renders the menu item, even if it is not displayed.
         }
      } :: Nil
   }
*/
   // part of ...
   def playerChoseFirstConstitution:Boolean =
   {  val sesCoordLR = sesCoord.is // <&y2012.08.08.20:19:05& temporary alternative, remove in future and do with persistent fields of Player>
      Player.currentUser match
      {  case Full(player) => {  if( sesCoord.constiSelectionProcedure == OneToStartWith) /* TODO check whether number of session played < N, then make true, otherwise false */ !sesCoord.firstChosenConstitution.isEmpty
                                 else false
                              }
         case _            => false
      }
   }

   def playedSessions = 1 // <&y2012.08.11.14:30:26& TODO implement>

    def sitemap() = SiteMap(
      Menu("Home") / "index" >> Player.AddUserMenusAfter, // Simple menu form
      Menu(Loc("Help", "help" :: Nil, "Help")),
      Menu(Loc("Constitutions", "constitutions" :: Nil, "Constitutions", If(() => ( playedSessions > 10 ), () => RedirectResponse("/index")) ) ),
      Menu(Loc("Study Constitution", "studyConstitution" :: Nil, "Study Chosen Constitution", If(() => playerChoseFirstConstitution, () => RedirectResponse("/index")) )),
      Menu(Loc("startSession", "constiTrainingDecision" :: Nil, "Play", If(() => {val t = Player.currentUser.isDefined; err.println("Menu Loc \"startSession\": user logged in = " + t); t}, () => RedirectResponse("/index")))),
      Menu(Loc("playerStats", "playerStats" :: Nil, "Your stats", If(() => {Player.currentUser.isDefined}, () => RedirectResponse("/index")))),
      Menu(Loc("all", Nil -> true, "If you see this, something is wrong: should be hidden", Hidden))
      )

    LiftRules.setSiteMapFunc(() => Player.sitemapMutator(sitemap()))

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => Player.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
/*   
    def loadSesHisPlayer(l: LiftSession, r: Req) = 
    { 
    }
*/
/*
    def autoLoginTestUser(l:LiftSession, r: Req) =
    { Player.logUserIdIn("1")
    }
  */  
    //if(TestSettings.AUTOLOGIN) {LiftSession.afterSessionCreate = ((l:LiftSession,r:Req)=>(println)) :: LiftSession.afterSessionCreate}
    if(TestSettings.AUTOLOGIN) { LiftSession.afterSessionCreate ::= ( (l:LiftSession, r: Req) => Player.logUserIdIn("1") ) }

    // Initialisation/shutdown code for OCBKC stuffzzzzariowaikoeikikal
    Constitution.deserialize // when lift starts up (= running this boot method!) load all constitutions from permanent storage
    LiftRules.unloadHooks.append(() => Constitution.serialize) // when lift shuts down, store all constitution objects

    // Initialise git repository for constitutions if there isn't one created yet.
    // Check whether there is already git tracking
    val gitfile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + "/.git")
    if( gitfile.exists)
    { println("   .git file exists in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", so everyfthing is under (version) control, my dear organic friend...")
    }
    else
    { println("   .git file doesn't exist yet in " + GlobalConstant.CONSTITUTIONHTMLDIR + ", creating new git repo...")
      val jgitInitCommand:InitCommand = Git.init()
      jgitInitCommand.setDirectory(new File(GlobalConstant.CONSTITUTIONHTMLDIR))
      jgitInitCommand.call()
    }

 // <&y2012.08.04.19:33:00& perhaps make it so that also this rewrite URL becomes visible in the browser URL input line>

   def dispatch4ConstiTrainingDecision = 
   {  println("dispatch4ConstiTrainingDecision called")
      val sesCoordLR = sesCoord.is; // extract session coordinator object from session variable. <&y2012.08.04.20:20:42& MUSTDO if none exists, there is no player logged in, handle this case also>
      val player = sesCoordLR.currentPlayer
      sesCoordLR.constiSelectionProcedure match
      {  case OneToStartWith  =>
            if( sesCoordLR.firstChosenConstitution.isEmpty )
            {  println("   player has not chosen a constitution to study yet, so redirect to selectConstitution.")
               S.redirectTo("selectConstitution")
            }
            else
            {  println("   player has already selected a constitution in the past, so redirect to play the session!")
               S.redirectTo("startSession")
            }
         case proc            => { val msg = "constiSelectionProcedure " + proc.toString + " not yet implemented."; println("  " + msg); throw new RuntimeException(msg) }
      }
   }

   LiftRules.viewDispatch.append
   {  // This is an explicit dispatch to a particular method based on the path
      case List("constiTrainingDecision") =>
         Left(() => Full( dispatch4ConstiTrainingDecision ))
   }

    println("Boot.boot finished")
  }


  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}
