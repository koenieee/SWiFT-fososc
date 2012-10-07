package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor, By}
import _root_.java.sql.{Connection, DriverManager}
import System._
import _root_.org.ocbkc.swift.model._
import org.ocbkc.swift.global._
import org.ocbkc.swift.OCBKC._
import org.eclipse.jgit.api._
import java.io._
import org.ocbkc.swift.snippet.sesCoord
import scala.util.Random

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
    Schemifier.schemify(true, Schemifier.infoF _, Player, PlayerCoreContent_join, CoreContentMetaMapperObj)

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

   // returns also false when no player is logged in.
   // this method ALWAYS returns None, it seems sesCoord.set_? is always false when this method is called (suggesting that sesCoord has not been created yet). Only fix this bug when I still use sesCoord (I'm planning to move to the Mapper framework for persistency), otherwise dissmiss it.
   def playerLoggedInAndChoseFirstConstitution:Boolean =
   {  println("Boot.playerLoggedInAndChoseFirstConstitution called")
      val r =  playerIsLoggedIn &&
               {  Player.currentUser match
                  {  case Full(player) => player.firstChosenConstitution != -1 // <&y2012.09.04.19:16:22& how to check that this MappedInt is indeed set? Now doing it with the protocol that -1 means not defined.>
                     case _            => throw new RuntimeException("   no player found.") // This cannot happen.
                  }
               }
      println("   return value = " + r)
      r
   }

   def playerIsAdmin(player:Player):Boolean =
   {  player.firstName.is.equals(GlobalConstant.ADMINFIRSTNAME)
   }

   // assumes playerIsLoggedIn
   def loggedInPlayerIsAdmin:Boolean =
   {  playerIsAdmin(Player.currentUser.open_!)
   }

  // returns -1 when no player is logged in
  // <&y2012.08.29.23:09:13& optimisation possible here (and in other parts of code), now the check "playerIsLoggedIn" is done over and over. Do it only once. E.g. by assuming in this and other methods that the player is already logged in.>
   def playedSessions:Long = 
    { val r = if(playerIsLoggedIn)
      {  val sesCoordLR = sesCoord.is
         sesCoordLR.sesHis.totalNumber
      }
      else
         -1

      println("   playedSessions = " + r)
      r
    }

    def playerIsLoggedIn:Boolean = 
    { val r = Player.currentUser.isDefined
      r
    }

    def sitemap() = SiteMap(
      Menu("Home") / "index" >> Player.AddUserMenusAfter, // Simple menu form
      Menu(Loc("Help", "help" :: Nil, "Help")),
      Menu(Loc("Constitutions", "constitutions" :: Nil, "Constitutions", 
         If(() =>
         {  println("Loc(Constitutions) called")
            if(playerIsLoggedIn)
            {  val sesCoordLR = sesCoord.is
               val player = sesCoordLR.currentPlayer
               playerIsAdmin(player) ||
                  {  player.constiSelectionProcedure match
                     {  case OneToStartWith =>
                        {  playedSessions >= OneToStartWith.minSesionsB4access2allConstis
                        }
                        case NoProc => true
                        case proc   =>
                        {  val msg = "constiSelectionProcedure " + proc.toString + " not yet implemented."
                           println("  " + msg)
                           throw new RuntimeException(msg)
                           false // will not be reached but for type correctness?
                        }
                     }
                  }
            }
            else
               false            
         },
         () => RedirectResponse("/index")) ) ), // <&y2012.08.11.19:22:55& TODO change, now I assume always the same constiSelectionProcedure>
      Menu(Loc("Study Constitution", "studyConstitution" :: Nil, "Study Chosen Constitution",
         If(() => {  val r = ( playerLoggedInAndChoseFirstConstitution &&
                        ( playedSessions < OneToStartWith.minSesionsB4access2allConstis ) )
                     println(" Loc(Study Constitution) access = " + r)
                     r 
                  },
            () => RedirectResponse("/index")
           ))), // <&y2012.08.11.19:23& TODO change, now I assume always the same constiSelectionProcedure>
      Menu(Loc("startSession", "constiTrainingDecision" :: Nil, "Play", If(() => {val t = playerIsLoggedIn && !loggedInPlayerIsAdmin; err.println("Menu Loc \"startSession\": user logged in = " + t); t}, () => RedirectResponse("/index")))),
      Menu(Loc("playerStats", "playerStats" :: Nil, "Your stats", If(() => playerIsLoggedIn && !loggedInPlayerIsAdmin, () => RedirectResponse("/index")))),
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
      val sesCoordLR = sesCoord.is // extract session coordinator object from session variable. <&y2012.08.04.20:20:42& MUSTDO if none exists, there is no player logged in, handle this case also>
      val player = sesCoordLR.currentPlayer

      player.constiSelectionProcedure match
      {  case OneToStartWith  =>
            if( player.firstChosenConstitution.is == -1 )
            {  println("   player has not chosen a constitution to study yet, so redirect to selectConstitution.")
               S.redirectTo("selectConstitution")
            }
            else
            {  println("   player has already selected a constitution in the past, so redirect to play the session!")
               S.redirectTo("startSession")
            }
         case NoProc          => S.redirectTo("startSession")
         case proc            => { val msg = "constiSelectionProcedure " + proc.toString + " not yet implemented."; println("  " + msg); throw new RuntimeException(msg) }
      }
   }

   LiftRules.viewDispatch.append
   {  // This is an explicit dispatch to a particular method based on the path
      case List("constiTrainingDecision") =>
         Left(() => Full( dispatch4ConstiTrainingDecision ))
   }
   
   println("   check whether admin account exists, if not: create it (yes, I feel just like God)...")
   Player.find(By(Player.firstName, GlobalConstant.ADMINFIRSTNAME)) match
   {  case Full(player) => {  println("   Admin account already exists, my beloved friend.")
                              Unit 
                           } // do nothing, player exists.
      case _            => {  println("   Doesn't exist: creating it...")
                              val p = Player.create.firstName(GlobalConstant.ADMINFIRSTNAME).email("cg@xs4all.nl").password("asdfghjk").superUser(true).validated(true)  // <&y2012.08.30.20:13:36& TODO read this information from a property file, it is not safe to have it up here (in open source repo)>
                              p.save
                           }
   }
      
   // TODO: before doing this, erase all persistency information, but not without a warning to the developer

   if(TestSettings.CREATETESTUSERBASE)
   {  Player.create.firstName("Aap1").email("aap1@test.org").password("asdfghjk").validated(true).save
      Player.create.firstName("Aap2").email("aap2@test.org").password("asdfghjk").validated(true).save
   }

   if(TestSettings.CREATEDUMMYCONSTITUTIONS)
   {  // <&y2012.09.15.20:58:31& erase persistent info first, but not before warning to developer>
      // NOTE: Now you HAVE to do that manually!
      val p1 = Player.create.firstName("Aap1").email("aap1@test.org").password("asdfghjk").validated(true)
      val p2 = Player.create.firstName("Aap2").email("aap2@test.org").password("asdfghjk").validated(true)
      p1.save; p2.save
      
      // randomly create between 
      val minconstis = 5
      val maxconstis = 50
      val minhis = 1
      val maxhis = 50
      
      val randomSeq = new Random()
      val numconstis = minconstis + randomSeq.nextInt(maxconstis - minconstis)
      def randomSizeHis = minhis + randomSeq.nextInt(maxhis - minhis)
      def randomPlayer:Player =
      {  val p = pickRandomElementFromList(Player.findAll(), randomSeq).get // assumed may be that there are players
         println("   random player = " + p)
         p
      }
      
      // <&y2012.09.15.13:34:56& move to general lib>
      def pickRandomElementFromList[A](list:List[A], rs:Random):Option[A] =
      {  list match
         {  case Nil => None
            case _   => Some(list(rs.nextInt( list.length - 1 )))
         }
      }

      val randomConstiCreationList = List.fill(numconstis)((randomPlayer, randomSizeHis))
      println("randomConstiCreationList =")
      println(randomConstiCreationList)
      randomConstiCreationList.foreach( { case (creator, sizeHis) => generateConstiHis(creator, sizeHis) } )

      def generateConstiHis(creator: Player, sizeHis:Int) =
      {  val consti = Constitution.create(creator.id.is)
         val randomHisCreationList = (1, creator)::List.range(2, sizeHis).map( idx => (idx, randomPlayer)) // Note: the first publication is always by the creator...
         randomHisCreationList.map( 
            { case (idx, publisher) =>            
               consti.publish(
"""<h2>Article 1</h2>

<p>publication """ + idx + """</p>
""", "publication " + idx, publisher.id.toString)
            }
         )
         Unit
      }
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
