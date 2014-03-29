package bootstrap.liftweb

import scala.xml._
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
import org.ocbkc.swift.OCBKC.OCBKCinfoPlayer._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.persist.PersDataUpgrader4SWiFT
import org.ocbkc.swift.jgit.InitialiseJgit
import java.io._
import org.ocbkc.swift.snippet.SesCoord
import scala.util.Random
import _root_.net.liftweb.widgets.tablesorter.TableSorter
import org.ocbkc.swift.test._
import org.ocbkc.swift.test.Types._
import org.ocbkc.swift.test.TestHelpers._
import org.ocbkc.swift.coord._
import org.ocbkc.swift.coord.ses._

import org.ocbkc.generic.random._
import ocbkc.swift.test.simulation.jara._
import net.liftmodules.JQueryModule

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
	JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
	JQueryModule.init()
   log("Boot.boot called")
   
   PersDataUpgrader4SWiFT.initialise(GlobalConstant.PERSISTENT_DATA_MAIN_VERSION_PATHNAME, GlobalConstant.MAIN_VERSION)
   PersDataUpgrader4SWiFT.apply

   LiftRules.useXhtmlMimeType = false
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    /* todo, not yet used, build this in at a later stage
    val roles =
      AuthRole("ForbiddenRole", // no one should ever have this role, because both admin and player have possibilities which the other role hasn't
        AuthRole("Admin"),
        AuthRole("Player")
        )
    */

    // where to search snippet
    LiftRules.addToPackages("org.ocbkc.swift")

    Schemifier.schemify(true, Schemifier.infoF _, Player, PlayerSessionInfo_join, SessionInfoMetaMapperObj, FollowerConsti_join)

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
   {  log("studyConstitutionLink called")
      "studyConstitution?id=" + 
      {  Player.currentUser match 
         {  case Full(player) =>
               player.firstChosenConstitution match
               {  case Some(const) => const.id.toString
                  case _           => log("   BUG: no first chosen constition found"); "BugNoFirstChosenConstitutionFound"
               }
            case _              => log("   BUG: no player found"); "BugNoPlayerFound" // or is this no bug? Perhaps lift always renders the menu item, even if it is not displayed.
         }
      } :: Nil
   }
*/
   // part of ...

   /** Only call when you are 100% certain a user is logged in, otherwise this must be considered a bug
     */
   def currentPlayer =
   {  Player.currentUser.get
   }

   // returns also false when no player is logged in.
   // this method ALWAYS returns None, it seems SesCoord.set_? is always false when this method is called (suggesting that SesCoord has not been created yet). Only fix this bug when I still use SesCoord (I'm planning to move to the Mapper framework for persistency), otherwise dissmiss it.
   def playerLoggedInAndChoseFirstConstitution:Boolean =
   {  log("Boot.playerLoggedInAndChoseFirstConstitution called")
      val r =  playerIsLoggedIn &&
               {  Player.currentUser match
                  {  case Full(player) => player.firstChosenConstitution != -1 // <&y2012.09.04.19:16:22& how to check that this MappedInt is indeed set? Now doing it with the protocol that -1 means not defined.>
                     case _            => throw new RuntimeException("   no player found.") // This cannot happen.
                  }
               }
      log("   return value = " + r)
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
      {  val sesCoordLR = SesCoord.is
         sesCoordLR.sesHis.totalNumber
      }
      else
         -1

      log("   playedSessions = " + r)
      r
    }

    def playerIsLoggedIn:Boolean = 
    { val r = Player.currentUser.isDefined
      r
    }

   //def playerIsLoggedInAndPlayed minSessionsPerPlayer

    def sitemap() = SiteMap(
      Menu("Home") / "index" >> Player.AddUserMenusAfter, // Simple menu form
      Menu(Loc("Help", "generalHelp" :: Nil, "Help")),
      //Menu(Loc("About", "aboutPage" :: Nil, "About")),
      Menu(Loc("Constitutions", "constitutions" :: Nil, "Constitutions", 
         If(() =>
         {  log("Loc(Constitutions) called")
            if(playerIsLoggedIn)
            {  val sesCoordLR = SesCoord.is
               val player = sesCoordLR.currentPlayer
               playerIsAdmin(player) ||
                  {  player.constiSelectionProcedure match
                     {  case OneToStartWith | RandomOneToStartWith =>
                        {  playedSessions >= OneToStartWith.minSessionsB4access2allConstis
                        }
                        case NoProc => true
                        case proc   =>
                        {  val msg = "constiSelectionProcedure " + proc.toString + " not yet implemented."
                           log("  " + msg)
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
                        ( playedSessions < OneToStartWith.minSessionsB4access2allConstis ) )
                     log(" Loc(Study Constitution) access = " + r)
                     r 
                  },
            () => RedirectResponse("/index")
           ))), // <&y2012.08.11.19:23& TODO change, now I assume always the same constiSelectionProcedure>
      Menu(Loc("startSession4OneToStartWith", "constiTrainingDecision" :: Nil, "Start Fluency Session", If(() => {val t = playerIsLoggedIn && !loggedInPlayerIsAdmin; log("Menu Loc \"startSession\": user logged in = " + t); t && ( currentPlayer.constiSelectionProcedure == OneToStartWith ) && (SesCoord.is.latestRoundFluencySession == NotInFluencySession)}, () => RedirectResponse("/index")))),
      Menu(Loc("startSessionOtherwise", "fluencyGameSes" :: "startSession" :: Nil, "Start Fluency Session", If(() => {val t = playerIsLoggedIn && !loggedInPlayerIsAdmin; log("Menu Loc \"startSession\": user logged in = " + t); t && ( currentPlayer.constiSelectionProcedure != OneToStartWith ) && (SesCoord.is.latestRoundFluencySession == NotInFluencySession)}, () => RedirectResponse("/index")))),
      Menu(Loc("continueSession", "continueFluencySession" :: Nil, "Continue Fluency Session", If(() => {val t = playerIsLoggedIn && !loggedInPlayerIsAdmin && (SesCoord.is.latestRoundFluencySession != NotInFluencySession); log("Menu Loc \"startSession\": user logged in = " + t); t}, () => RedirectResponse("/index")))),
      Menu(Loc("playConstiGame", "constiGame" :: Nil, "Play ConstiGame", If(() => {val t = playerIsLoggedIn && !loggedInPlayerIsAdmin; log("Menu Loc \"startSession\": user logged in = " + t); t}, () => RedirectResponse("/index")))),
      Menu(Loc("playerStats", "playerStats" :: Nil, "Your stats", If(() => playerIsLoggedIn && !loggedInPlayerIsAdmin, () => RedirectResponse("/index")))),
      Menu(Loc("AdminPage", "adminPage" :: Nil, "Admin Control", If(() => playerIsLoggedIn && loggedInPlayerIsAdmin, () => RedirectResponse("/index")))),
      Menu(
         Loc(
            "constitution",
            "constitution" :: Nil,
            "If you see this, something is wrong: should be hidden",
            List( Hidden,
               If( () => ( playerIsLoggedIn && ( playerHasAccessToAllConstis(currentPlayer) || playerIsAdmin(currentPlayer) )) , () => RedirectResponse("/index"))
            )
         )
      ),
      Menu(
         Loc(
            "history",
            "history" :: Nil,
            "If you see this, something is wrong: should be hidden",
            List( Hidden,
               If( () => ( playerIsLoggedIn && ( playerHasAccessToAllConstis(currentPlayer) || loggedInPlayerIsAdmin ) ), () => RedirectResponse("/index"))
            )
         )
      ),
      Menu(
         Loc(
            "fluencyGameSes",
            new Link("fluencyGameSes" :: Nil, true),
            "If you see this, something is wrong: should be hidden",
            List( Hidden,
               If( () => ( playerIsLoggedIn && !playerIsAdmin(currentPlayer) ), () => RedirectResponse("/index"))
            )
         )
      )
   ,
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
    //if(TestSettings.AUTOLOGIN) {LiftSession.afterSessionCreate = ((l:LiftSession,r:Req)=>(log)) :: LiftSession.afterSessionCreate}
    if(TestSettings.AUTOLOGIN.ON) { LiftSession.afterSessionCreate ::= ( (l:LiftSession, r: Req) => Player.logUserIdIn(TestSettings.AUTOLOGIN.USER_ID) ) }

    // Initialisation/shutdown code for OCBKC stuffzzzzariowaikoeikikal
    Constitution.deserialize // when lift starts up (= running this boot method!) load all constitutions from permanent storage
    LiftRules.unloadHooks.append(() => Constitution.serialize) // when lift shuts down, store all constitution objects

   InitialiseJgit()

 // <&y2012.08.04.19:33:00& perhaps make it so that also this rewrite URL becomes visible in the browser URL input line>



   val lvd = LiftRules.viewDispatch

   lvd.append
   {  case List("continueFluencySession") =>
         Left(() => Full( BootHelpers.continueOrStartFluencySession ))
   }

   log("   check whether admin account exists, if not: create it (yes, I feel just like God)...")
   val admin = Player.find(By(Player.firstName, GlobalConstant.ADMINFIRSTNAME)) match
   {  case Full(player) => {  log("   Admin account already exists, my beloved friend.")
                              player 
                           } // do nothing, player exists.
      case _            => {  log("   Doesn't exist: creating it...")
                              val p = Player.create.firstName(GlobalConstant.ADMINFIRSTNAME).email("cg@xs4all.nl").password("asdfasdf").superUser(true).validated(true)  // <&y2012.08.30.20:13:36& TODO read this information from a property file, it is not safe to have it up here (in open source repo)>
                              p.save
                              p
                           }

   }

   GlobalConstant.adminOpt = Some(admin)


   // TODO: before doing this, erase all persistency information, but not without a warning to the developer
   if(TestSettings.CREATETESTUSERBASE)
   {  val randomSeq = new Random
      val numberOfPlayers = RandomExtras.nextBetween(randomSeq, 1, 1)
      log("   numberOfPlayers = " + numberOfPlayers)
      List.range(1, numberOfPlayers + 1).foreach(n => Player.create.firstName("Aap" + n).email("aap" + n + "@test.org").password("asdfasdf").validated(true).save)
   }

   if(TestSettings.CREATEDUMMYCONSTITUTIONS)
   {  // <&y2012.09.15.20:58:31& erase persistent info first, but not before warning to developer>
      // NOTE: Now you HAVE to do that manually!
      
      // randomly create between 
      val minconstis = 5
      val maxconstis = 50
      val minhis = 1
      val maxhis = 50
      
      val randomSeq = new Random
      val numconstis = minconstis + randomSeq.nextInt(maxconstis - minconstis)
      def randomSizeHis = minhis + randomSeq.nextInt(maxhis - minhis)
      def randomPlayer:Player =
      {  val p = RandomExtras.pickRandomElementFromList(Player.findAll, randomSeq).get // assumed may be that there are players
         log("   random player = " + p)
         p
      }
      

      val randomConstiCreationList = List.fill(numconstis)((randomPlayer, randomSizeHis))
      log("randomConstiCreationList =")
      log(randomConstiCreationList.toString)
      randomConstiCreationList.foreach( { case (creator, sizeHis) => generateConstiHis(creator, sizeHis) } )

      def generateConstiHis(creator: Player, sizeHis:Int) =
      {  val consti = Constitution.create(creator.id.is)
         consti.initialiseNew
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

   // create constitution alpha from the initialisationData dir, only if there exist no constitutions yet (otherwise, assume that it has been created (and not deleted) since the last time the application was up)
   Constitution.createConstiAlphaIfDoesntExist

   if(TestSettings.SIMULATEPLAYINGWITHJARA)
   {  TestSettings.SIMULATECLOCK = true
      TestSettings.SIMULATEPLAYINGWITHJARARUNNING = true // <_&y2013.02.11.12:15:09& refactor: better put this (also) in PlayingSimulator.start? This is  bug prone - if you forget to set it, same holds for SIMULATECLOCK.>[A &y2013.04.15.19:57:38& this has been done in another branch, merge it]

      PlayingSimulator.start(45000)
      TestSettings.SIMULATECLOCK = false
      TestSettings.SIMULATEPLAYINGWITHJARARUNNING = false

   }

   if(TestSettings.SIMULATEPLAYINGWITHFIRSTSIMSYSTEM)
   {  /* This simulation is not intended to simulate all aspects (at least not in the current stage, it may be later extended). It should for now be sufficient to test constitutional scoring calculation. Assumptions now are:
            - Users have already been created
            - Constitutions have already been created
            - Each user works in one simulated "session" (they don't log off, and only have one session per user)
      
         strategy:
         - First create a sequence of events (simply a list). After that "played" the list of events. The events are sorted by time they happen, and they are tupled with the simulated time at which they should occur. E.g. List( ( player 1 chooses constitution c1, time = 1s), (player 2 , etc.) )
         - An idea may be that the events are simply the function calls themselves!
         - The order of creation of the sequence is:
            - for each player, create the complete sequence of events for that player, which consists of:
               - choose a constitution
               - play a session
               - repeat the previous step a random number of times
            - then calculate the union of the event-sequences of each player, sorted by event time.
      */
      log("TestSettings.SIMULATEPLAYING set, so test now carried out...")
      TestSettings.SIMULATECLOCK = true

      // >>> Configuration of test
      // TODO: move to GlobalConstants
      val minSessionsPerPlayer = 0
      val maxSessionsPerPlayer = 8 // perhaps relate to minSesionsB4access2allConstis
      val minTimeBeforeChoosingConsti = 1000 * 5 // ms
      val maxTimeBeforeChoosingConsti = 1000 * 60 * 60 * 24 // ms
      val minTimeBetweenSessions = 1000 * 5 // ms
      val maxTimeBetweenSessions = 1000 * 60 * 60 // ms
      val minDurationTranslation = 1000 * 10 // ms
      val maxDurationTranslation = 1000 * 60 * 20 // ms
      val minDurationAlgoDef = 1000 * 2 // ms
      val maxDurationAlgoDef = 1000 * 60 // ms

      // subfunctions

      /**  @param f: function which maps element if inList (A) to value of type C, and gets a value of type B as context information originating from the previous time f was applied to the element at the left. 
        *
        */
      // <&y2012.10.23.23:40:37& todo: move to general lib>
      def mapWithLeftContext[A,B,C](inList:List[A], leftContext:B, f:(A,B) => (C,B) ):List[C] =
      {  inList match
         {  case x::xs  => {  val (newX, nextLeftContext) = f(x,leftContext)
                              newX::mapWithLeftContext(xs, nextLeftContext, f)
                           }
            case List() => List()
         }
      }

      val randomSeq = new Random

      def simulatePlayingSessions(p:Player, numberOfSessions:Int):List[DelayedSimulatedEvent] =
      {  // create simulated session for player
         val sesCoordLR = new ses.CoreSimu(p)
      
         //- simulate choosing a constitution to play with (also see selectConstitution.scala)
         if(Constitution.count < 1) throw new RuntimeException("simulatePlayingSessions: No constitutions created yet")

         val randomConstiId = 1 + randomSeq.nextInt(Constitution.count - 1)
         
         val chooseConstiEvent = List((randomPause(minTimeBeforeChoosingConsti, maxTimeBeforeChoosingConsti, randomSeq), () => sesCoordLR.URchooseFirstConstitution(randomConstiId)))

         // simulate playing sessions
         val sessionsEvents = if( numberOfSessions > 0 )
         {  val sessionIndices = List.range(0, numberOfSessions-1)
            
            def f(sessionIndex:Int, endTimeLastSession:Long):(List[DelayedSimulatedEvent], Long) =
            {  val session = simulatePlayingSession(p, endTimeLastSession, sesCoordLR)
               val endTime = 0L // TODOextractEndTime(session)
               (session, endTime)
            }

            mapWithLeftContext(sessionIndices, 0L, f).flatten
         }
         else List()
      
         val ret = chooseConstiEvent ++ sessionsEvents
         log("   ret = " + chooseConstiEvent)
         ret
      }

      def simulatePlayingSession(p:Player, startAfter:Long, sesCoordLR:ses.CoreSimu):List[DelayedSimulatedEvent]  =
      {  log("simulatePlayingSession called")
         log("[MUSTDO] Jara simulation needsrefactoring because of added round StudyConstiRound")
         val winSession = randomSeq.nextBoolean
         List( 
            (randomPause(minTimeBetweenSessions, maxTimeBetweenSessions, randomSeq), () => sesCoordLR.URtryStartSession ),
            (randomPause(minDurationTranslation, maxDurationTranslation, randomSeq), () => sesCoordLR.URstopTranslation ),
            (randomPause(minDurationAlgoDef, maxDurationAlgoDef, randomSeq), () => sesCoordLR.URalgorithmicDefenceSimplified(winSession,25*1000))
         )
      }

      // <<< configuration of test


      // for each player, create the complete sequence of events for that player, which consists of:
     
      val players = Player.findAll

      val simulatedEventsGroupedByPlayer:List[List[DelayedSimulatedEvent]] = players.map( p => simulatePlayingSessions(p, randomSeq.nextInt(maxSessionsPerPlayer - minSessionsPerPlayer) ) )
      
      def toAbsoluteTimes(eventList:List[DelayedSimulatedEvent]) =
      {  def f(event:DelayedSimulatedEvent, cummulativeTime:Long):(DelayedSimulatedEvent, Long) =
         {  val newCummulTime = cummulativeTime + event._1
            ((newCummulTime, event._2), newCummulTime)
         }

         mapWithLeftContext(eventList, 0L, f)
      }

      val simulatedEvent = simulatedEventsGroupedByPlayer.map{ eventPlayer => toAbsoluteTimes(eventPlayer) }

      val simulatedEventsAbsoluteTimes = simulatedEventsGroupedByPlayer
                                          .map{ eventPlayer => toAbsoluteTimes(eventPlayer) }
                                          .flatten
                                          .sortWith{ case Tuple2(event1, event2) => (event1._1 < event2._1) }
      // COULDDO now turn eventList back into list with pause times between events instead of times from the start of the event queue, and adapt runEvent (it simplifies runEvent).

      // run eventList
      def runSimulatedEvent(event:DelayedSimulatedEvent) =
      {  log("runSimulatedEvent")
         SystemWithTesting.currentTimeMillis = SystemWithTesting.startTimeMillis_simu + event._1
         log("   time:" + SystemWithTesting.currentTimeMillis )
         log("   event:" + event._2 )
         event._2()
      }   


         
      simulatedEventsAbsoluteTimes.map(runSimulatedEvent)

      // TODO: replace result type with more specific type if possible
      TestSettings.SIMULATECLOCK = false
   }

   // initialise widgets
   TableSorter.init

   // make it possible to inspect lift database by going to server-address/console
   if (Props.devMode || Props.testMode) {
      log("   make it possible to inspect lift database by going to server-address/console")
     LiftRules.liftRequest.append({case r if (r.path.partPath match {
       case "console" :: _ => true
       case _ => false}
     ) => false})
   }

      log("Boot.boot finished")
  }


  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }
}

/** Added for SWiFT
  */

object BootHelpers
{  def dispatch4ConstiTrainingDecision = 
   {  log("dispatch4ConstiTrainingDecision called")
      val sesCoordLR = SesCoord.is // extract session coordinator object from session variable. <&y2012.08.04.20:20:42& MUSTDO if none exists, there is no player logged in, handle this case also>
      val player = sesCoordLR.currentPlayer

      player.constiSelectionProcedure match
      {  case OneToStartWith  =>
            if( player.firstChosenConstitution.is == -1 )
            {  log("   player has not chosen a constitution to study yet, so redirect to selectConstitution.")
               S.redirectTo("fluencyGameSes/selectConstitution")
            }
            else
            {  log("   player has already selected a constitution in the past, so redirect to start/continue the session!")
               log("[SHOULDDO] refactor the dispatching (= continuing session) in this case, this is needed more often..")
               continueOrStartFluencySession
            }
         case _ => 
         {  log("[BUG] this method should only be called iff OneToStartWith is the case.");
            S.redirectTo("fluencyGameSes/startSession")
         }
      }
      NodeSeq.Empty // this will never be reached because of the redirects.
   }
  
   def continueOrStartFluencySession =
   {  val lrfs = SesCoord.is.latestRoundFluencySession
      log("   latestRoundFluencySession = " + lrfs)
      lrfs match
      {  case NotInFluencySession => S.redirectTo("fluencyGameSes/startSession")
         case RoundConstiStudy => S.redirectTo("fluencyGameSes/studyConstiRound")  
         case RoundFinaliseSession => S.redirectTo("fluencyGameSes/finaliseSession")
         case RoundTranslation => S.redirectTo("fluencyGameSes/translationRound")
         case RoundBridgeConstruction => S.redirectTo("fluencyGameSes/bridgeconstruction_efe")
         case RoundQuestionAttack => S.redirectTo("fluencyGameSes/questionAttackRound")
         case RoundAlgorithmicDefenceStage1 => S.redirectTo("fluencyGameSes/algorithmicDefenceRound")
         case RoundAlgorithmicDefenceStage2 => S.redirectTo("fluencyGameSes/algorithmicDefenceRoundStage2")
         case _                   => log("[BUG] implement the rest")
      }
      NodeSeq.Empty
   }
}
