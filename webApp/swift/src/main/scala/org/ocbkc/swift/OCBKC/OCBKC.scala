package org.ocbkc.swift.OCBKC
{
import _root_.scala.xml._
import System._
import org.ocbkc.swift.cores.{TraitGameCore, NotUna}
import org.ocbkc.swift.cores.gameCoreHelperTypes._
import org.ocbkc.swift.global._
import org.ocbkc.swift.global.Logging._
import org.ocbkc.swift.global.ScalaHelpers._
import net.liftweb.json._
import java.io._
import java.util.Date
import org.ocbkc.generic.DateTime._
import org.apache.commons.io.filefilter._
import net.liftweb.common.{Box,Empty,Failure,Full}
import org.ocbkc.swift.parser._
import org.ocbkc.swift.model.Player
import GlobalConstant.jgit
import org.ocbkc.swift.jgit._
import org.ocbkc.swift.jgit.JgitUtils._
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib._
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.gitective.core.BlobUtils
import org.xml.sax.SAXParseException
import GlobalConstant.jgitRepo
import net.liftweb.mapper._
import scala.util.matching._
import scala.util.matching.Regex._
import org.ocbkc.swift.model._
import org.ocbkc.swift.jgit.Translations._
import org.ocbkc.swift.test.SystemWithTesting
import org.ocbkc.swift.test.TestHelpers._

/* Conventions:
Abbreviation for constitution: consti (const is to much similar to constant).

*/
object TestSerialization
{  def main(args: Array[String]) =
   {  if( args.length != 0 ) log("Usage: command without arguments")
      val const1 = Constitution(1,15,2,0,"Lets go organic!",None,List(),List(2),None,None,None)
      const1.serialize
   }
}

object ConstitutionTypes
{  type ConstiId  = Int
   type VersionId = String // which should be a git-commit hash.
}

import ConstitutionTypes._

/* <& &y2012.06.03.13:36:12& how to turn this into a complete case class, so only using vals, also for things that (may or will) change such as the average score, description etc?>
   <? &y2012.06.03.14:21:23& previous>
   
*/

/* Perhaps for future work:
case class ConstitutionVersion(val consti:Constitution, val version:VersionId)
*/

case class ReleaseStatus

abstract case class PotentialRelease extends ReleaseStatus 
{  log("[MUSTDO] &y2013.05.23.00:54:41& problem with serialization of PotentialRelease, I think lift-json can't deal with abstract case classes. Perhaps try to remove the \"abstract\" keyword. Not elegant, because it should be abstract.")
}
case object ReleaseCandidate extends PotentialRelease // this will become a release virgin if there are no prior releases yet, or the previous release has received its score.
case object ReleaseVirgin extends PotentialRelease // this is almost a release: as soon as the first player chooses it, it becomes a release.
case object Release extends ReleaseStatus

// <&y2012.09.18.10:39:31& IMPORTANT: currently the following class doesn't make use of its integration in the Mapper framework (the LongKeyedMetaMapper extension).
/** @param leadersUserIDs the user ids of the leaders of this constitution. By default the person who has create this constitution.
  * @todo &y2013.05.06.11:09:01& COULDDO make intelligent setter for releaseStatusLastVersion, which will also change releaseStatusPotentialRelease, and vice versa.
  */
case class Constitution(val constiId:ConstiId, // unique identifier for this constitution <&y2012.08.28.21:16:10& TODO refactor: use id of Mapper framework>
                        val creationTime:Long, // creationTime in unix time in seconds
                        val creatorUserID:Long,
                        var averageScore:Int, // redundant, for efficiency
                        var shortDescription:String,
                        val predecessorId:Option[ConstiId],
                        var followers:List[Long], // followers are users following this constitution. This includes optional features such as receiving emails when an update is made to that constitution etc. /* TODO &y2013.01.29.10:27:4 better to change into direct Player-objects */
                        var leadersUserIDs:List[Long],
                        var releaseStatusLastVersion:Option[ReleaseStatus],
                        var _commitIdPotentialRelease: Option[VersionId], // use commitIdPotentialRelease instead of this one
                        var releaseStatusPotentialRelease:Option[PotentialRelease]
                       )// extends LongKeyedMapper[Constitution] with IdPK
{  import scoring._
   val RELEASE_CANDIDATE_TAG_STRING = "consti" + constiId + ".releaseCandidate" 
   val RELEASE_VIRGIN_TAG_STRING = "consti" + constiId + ".releaseVirgin"
   val RELEASE_TAG_STRING = "consti" + constiId + ".release"
   //def getSingleton = ConstitutionMetaMapperObj
   val htmlFileName = "constitution" + constiId + ".html"
   
   var commitIdsReleases:List[String] = Nil // a list of commit id's constituting the released versions. WARNING: from newest to oldest. Newest this is first in list.

   def commitIdPotentialRelease_=(commitId:Option[VersionId])
   {  log("commitIdPotentialRelease_= called")
      log("   commitId = " + commitId)
      _commitIdPotentialRelease = commitId
   }

   def commitIdPotentialRelease:Option[VersionId] =
   {  _commitIdPotentialRelease
   }
   /** first release has number 1
     */
   def releaseIndex(releaseId:VersionId) = 
   {  commitIdsReleases.size - commitIdsReleases.indexOf(releaseId)
   }
   
   def releasesExist = (commitIdsReleases != Nil)

   // <&y2012.12.07.20:25:56& MUSTDO optimization necessary (function memoization)? In this way it is probably very costly...>
   def latestRevCommit:Option[RevCommit] =
   {  val h = getHistory

      if( h.size > 0)
         Some(getHistory(0))
      else
         None
   }

   def firstReleaseExists = 
   {  log("firstReleaseExists")
      val r = ( commitIdsReleases != Nil )
      log("   returns: " + r)
      r
   }

   def lastReleaseCommitId:Option[String] =
   {  commitIdsReleases match
      {  case Nil          => None
         case newest::tail => Some(newest)
      }
   }

   def lastReleaseVersionInfo:Option[ConstiVersionInfo] =
   {  lastReleaseCommitId match
      {  case Some(commitid)     => getConstiVersionInfo(commitid)
         case None         => None
      }
   }

   /** This constitution is a completely new one - initialise it from scratch.
     */
   def initialiseNew =
   {  // create html file which holds the constitution
      save(Constitution.templateNewConstitution(constiId))
   }
   /* <&y2012.06.02.20:19:54& optimisations needed, and if so, how would be best? Now it loads the html at each call> */
   def plainContent:String =
   {  val content:String = scala.io.Source.fromFile(GlobalConstant.CONSTITUTIONHTMLDIR + htmlFileName).mkString
      content
   }

   def plainContentLastRelease:String =
   {  "not implemented yet"
   }

   def followersAsPlayerObjs =
   {  followers.map{ Player.find(_).open_! }
   }
/* &y2012.06.23.14:39:56& just use followers.contains(...) directly, instead of:
   def followedByUser/isFollower(userId:Int):boolean = 
   {  followers.contains(userId) 
   }
*/
   def isRelease(commitId:String) =
   {  !commitIdsReleases.find(_.equals(commitId)).isEmpty
   }
  

   def releaseStatus(commitId:String):Option[ReleaseStatus] =
   {  log("releaseStatus called")
      val tags = JgitUtils.tagsOf(commitId)
      val ret = if(tags.exists{ tag => tag.getName.startsWith(RELEASE_TAG_STRING)  })
         Some(Release)
      else if(tags.exists{ tag => tag.getName.startsWith(RELEASE_VIRGIN_TAG_STRING)  })
         Some(ReleaseVirgin)
      else if(tags.exists{ tag => tag.getName.startsWith(RELEASE_CANDIDATE_TAG_STRING)  })
         Some(ReleaseCandidate)
      else
         None
      log("   returning: " + ret)
      ret
   }

   // <&y2012.06.12.21:35:34& optimise: only reload when something changed>
   def contentInScalaXML:Elem =
   {  plainTextXMLfragment2ScalaXMLinLiftChildren(plainContent)
   }

   // None means: no version released yet.
   def contentLastReleaseInScalaXML:Option[Elem] =
   {  lastReleaseCommitId match
      {  case None            => None
         case Some(commitid)  => Some(historicContentInScalaXML(commitid).get.content)
      }
   }

   def plainTextXMLfragment2ScalaXMLinLiftChildren(plain:String):Elem =
   {  val contentWrapped = "<lift:children>" + plain + "</lift:children>"
      log("   contentWrapped:String = " + contentWrapped)
      val xml = XML.loadString( contentWrapped )
      xml
   }
   
   // <&y2012.07.30.18:48:44& refactor this: put in more general lib, howver, then also the lift:children should be replaced by some generic XML construct>
   // <&y2012.07.30.20:06:22& optimse: use result of previous xml renderings if no updates where done, instead of doing XML.loadString again, and integrate this with other methods dealing with rendering XML if possible.>
   case class XMLandErr(val XML:Option[Elem], val exception:SAXParseException)

   def checkCorrectnessXMLfragment(xmlStr:String):XMLandErr =
   {  val contentWrapped = "<lift:children>" + xmlStr + "</lift:children>"
      log("   contentWrapped:String = " + contentWrapped)
      try
      {  XMLandErr( Some(XML.loadString( contentWrapped )), null)
      }
      catch
      {  case e:SAXParseException => XMLandErr(None, e)
      }
   }
   /** This method only saves the constitution text to a file (no git commit, no mapper framework, no jason)<? &y2012.05.28.16:36:27& what would be a good way to store constitutions? In a database? As files? In memory in a string field (of course not, but just to make my list complete)?> 
   */

   def save(constitutionText:String) =
   {  // throw new RuntimeException("Constitution.save is not yet implemented: use publish instead")
      // <&y2012.06.11.19:53:08& first add lift:children tag to make it a well-formed xml file.>
      val outFile = new File(GlobalConstant.CONSTITUTIONHTMLDIR + htmlFileName)

      log("  saving file: " + outFile.getAbsolutePath)
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.print(constitutionText)
      out.flush
      out.close
   }
   
   def currentVersionId:VersionId =
   {  // TODO
      "not implemented yet"
   }
   
   // Adds and commits constitutionText using jgit
   def publish(constitutionText:String, commitMsg:String, userId:String):Boolean =
   {  log("Constitution.publish called")
      save(constitutionText)

      val fullPath2Const = htmlFileName

      // now add and commit to git repo
      // &y2013.01.02.11:57:40& TODO: refactor code such that simulation aspects do not appear in the. Perhaps refactor Constitution.scala: put common aspects in trait, and make subclass SimulatedConstitution and normal? Or use reflection to rewrite code if there is no simulation?
      val gUserId = if( !TestSettings.SIMULATEPLAYINGWITHJARARUNNING )
                    {   gitUserId(userId) // <&y2012.06.30.19:16:16& later refactor with general methods for translating jgit name to lift user name back and forth>
                    }
                    else
                    {   gitUserId(userId, new Date(SystemWithTesting.currentTimeMillis))
                    }

      log("   jgit author id = " + gUserId.toString)
      log("   now adding and committing: " + fullPath2Const )
      //jgit.status
      val addCommand = jgit.get.add
      log("   isUpdate() (should be false) " + addCommand.isUpdate)
      addCommand.addFilepattern( fullPath2Const ).call
      val status = jgit.get.status.call
      log("   added files " + status.getAdded )
      log("   modified files " + status.getModified )
      log("   changed files " + status.getChanged )
      log("   untracked files " + status.getUntracked )
      // Determine whether this version will become the new release
         
      val revcom:RevCommit = jgit.get.commit.setAuthor(gUserId).setCommitter(gUserId).setMessage(commitMsg).call
/*
Try out:
getHistory.length, commitIdsReleases.length, isRelease
1 1 false
2 1 true
3 3 false
4 3 true
5 5 false
*/
      log("   getHistory.length = " + getHistory.length)
      log("   commitIdsReleases.length = " + commitIdsReleases.length)
      releaseStatusLastVersion = None
      log("[POTENTIAL_BUG] this method returned a boolean in the previous version, is this still needed?")
      false
   }


   /** WARNING: assumes that there is at least one release, otherwise considered as bug + exception. This may be handy to change in the future.
     *
     */
   def latestReleaseHasSufficientSampleSize:Boolean =
   {  // Determine whether sample size on the latest release without a fluency score is high enough

      ConstiScores.sampleSizeSufficient4FluencyScore(constiId).getOrElse(logAndThrow("[POTENTIAL_BUG]: assumes that there is at least one release, otherwise considered as bug + exception. This may be handy to change in the future."))
   }

   def restore(commitId:String, liftUserId:String) // <&y2012.07.23.17:17:39& better do resolving of commit-hash to commit-object here>
   {  val revcom:RevCommit = JgitUtils.revComFromCommitId(commitId).getOrElse(throw new RuntimeException("this commit id doesn't exist, should be impossible, though, 'cause you restore is always called with an existing commitId... >:-("))
   
      val gitUi = gitUserId(liftUserId)
      val dateFormat =  new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")

      val commitMsg = "Restored version of " + dateFormat.format(revcom.getCommitTime.toLong*1000)
      jgit.get.checkout.addPath(htmlFileName).setStartPoint(revcom).call
      jgit.get.commit.setAuthor(gitUi).setCommitter(gitUi).setMessage(commitMsg).call
   }

   /* Only serializes the object - not the html text which is already stored... */
   def serialize =
   {  implicit val formats = Serialization.formats(NoTypeHints)
      var constSer:String = Serialization.write(this)
      log("  Constitution-object serialised to: " + constSer)
      // write constitution-object to file with unique name

      var outFile = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/Constitution" + constiId)
      log("   creating file: " + outFile.getAbsolutePath)
      // outFile.getParentFile().mkdirs() these should already exist
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(constSer)
      out.close
   }
   
   case class ConstiVersionInfo(val commitId:String, val creationDatetimePOSIX:Long, val publisher:Player, val publishDescription:String)   
   // <&y2012.08.19.13:56:24& refactor rest of code to use this method instead of calling jgit on the spot (as long as that is not in conflict with efficiency issues)>
   def getConstiVersionInfo(commitId:String):Option[ConstiVersionInfo] =
   {  JgitUtils.revComFromCommitId(commitId) match
      {  case Some(revcom) =>
         {  val playerId = revcom.getAuthorIdent.getName
            val publisher:Player = Player.find(playerId) match
            {  case Full(player)  => player
               case _             => { val errmsg = "BUG: Player with this id " + playerId + " not found. No, I'm not angry, just very very disappointed..."; log("   " + errmsg); throw new RuntimeException(errmsg) }
            }
            Some(ConstiVersionInfo(commitId, revcom.getCommitTime.toLong, publisher, revcom.getFullMessage))
         }
         case None => None
      }
   }

   case class HisCon(val content:Elem, val creationDatetimePOSIX:Long)

   def historicContentInScalaXML(commitId:String):Option[HisCon] =
   {  val rw = new RevWalk(jgitRepo.get)
      val revcom:RevCommit = rw.parseCommit(ObjectId.fromString(commitId))
      val hisCon = BlobUtils.getContent(jgitRepo.get, revcom, htmlFileName)
      val hisConXML = plainTextXMLfragment2ScalaXMLinLiftChildren(hisCon)
      Some(HisCon(hisConXML, revcom.getCommitTime().toLong ))
   }

   /** @todo optimize this.
     */
   def getHistory:List[RevCommit] =
   {  import scala.collection.JavaConverters._
      jgit.get.log.addPath( htmlFileName ).call.asScala.toList
   }

   def isFirstPublication:Boolean =
   {  getHistory.size == 1
   }
   
   

   def addFollower(p:Player) = 
   {  if(!followers.contains(p.id.is))
      {  FollowerConsti_join.create.player(p).constiId(this.constiId).save
         followers = p.id.is::followers
      }
   }

   def removeFollower(p:Player):Unit = 
   {  getFollowerConsti_join(p) match
      {  case Some(fcj) => { if(!fcj.delete_!) throw new RuntimeException("Couldn't remove player " + p + " as follower from consti " + this + " from the database.") }
         case _         => log("   player wasn't a follower anyway, nothing to remove. Why did you disturb me from my Devine nap...")
      }
      followers = followers.filter(_ != p.id.is)
      Unit
   }

   def getFollowerConsti_join(p:Player):Option[FollowerConsti_join] =
   {  FollowerConsti_join.findAll(By(FollowerConsti_join.player, p), By(FollowerConsti_join.constiId, this.constiId)) match // <&y2012.11.28.23:32:32& not sure whether this works: By ..., By, what I want is an AND.
      {  case List(fcj)    => Some(fcj)
         case x::(y::xs)   => throw new RuntimeException("duplicate FollowerConsti_join in database.")
         case _            => None
      }
   }

   /** refactor move to general jgit lib
     */
   private def tagRevCommit(commitId:VersionId, name:String, message:String):Ref =
   {  tagRevCommit(JgitUtils.revComFromCommitId(commitId).get, name, message)
   }

   private def tagRevCommit(revCom:RevCommit , name:String, message:String):Ref =
   {  jgit.get.tag.setName(name).setObjectId(revCom).setTagger(GlobalConstant.adminGitUserId.get).setMessage("Make this version the release candidate (there may only be one!").call // <&y2012.08.22.16:52:30& perhaps change setTagger to some default system git-user account id, which is not tied to a player?
   }

   private def tagReleaseCandidate(commitId:VersionId):Ref =
   {  tagRevCommit(commitId, RELEASE_CANDIDATE_TAG_STRING, "")
   }

   private def tagReleaseVirgin(commitId:VersionId):Ref =
   {  tagRevCommit(commitId, RELEASE_VIRGIN_TAG_STRING, "")
   }

   private def tagRelease(commitId:VersionId):Ref =
   {  tagRevCommit(commitId, RELEASE_TAG_STRING + (commitIdsReleases.length + 1), "")
   }

   private def delTagReleaseCandidate =
   {  jgit.get.tagDelete.setTags(RELEASE_CANDIDATE_TAG_STRING).call
   }

   private def delTagReleaseVirgin =
   {  jgit.get.tagDelete.setTags(RELEASE_VIRGIN_TAG_STRING).call
   }
   /** turns current version into a release candidate. The current version is the last version that was committed with git. Non-git-committed changes will not be stored, and note that the constitution object cannot even see these, because they are entirely a matter of the GUI.
     */
   def makeLatestVersionReleaseCandidateIfPossible
   {  // make latest version the new release candidate
      log("makeLatestVersionReleaseCandidate called")
      log("   consti = " + constiId)

      log("   checking if possible...")
      latestRevCommit match
      {  case Some(lrc) =>
         {  val lcommitid = lrc.name

            def makeLatestVersionReleaseCandidate =
            {  log("makeLatestVersionReleaseCandidate called")
               unmakeCurrentPotentialRelease
               releaseStatusLastVersion = Some(ReleaseCandidate)
               releaseStatusPotentialRelease = Some(ReleaseCandidate)
               commitIdPotentialRelease = Some(lcommitid)

               tagReleaseCandidate(lcommitid)
            }

            if(releaseStatusLastVersion == None)
            {  log("    yes, possible, doing it!")
               makeLatestVersionReleaseCandidate
            }
            else
            {  log("   [POTENTIAL_BUG]: you tried to makeReleaseCandidate of a version which is already a release, release virgin or release candidate.")
               log("   but, hey, dudicon, still try to make a release candidate for it for you for it, good old chap...")
               makeLatestVersionReleaseCandidate
            }
         }
         case None => logAndThrow("[POTENTIAL_BUG] you tried to makeLatestVersionReleaseCandidate, while there is no version yet of this constitution?")
      }

      turnReleaseCandidateIntoVirginIfPossible
   }

   /**
     */
   def turnReleaseCandidateIntoVirginIfPossible
   {  log("turnReleaseCandidateIntoVirginIfPossible called")
      log("   consti = " + constiId)
      releaseStatusPotentialRelease match
      {  case Some(ReleaseCandidate) => 
         {  if( !releasesExist || latestReleaseHasSufficientSampleSize )
            {  log("   Yes, my dear organic friend! Possible! Doing it...")
               // remove release candidate status
               delTagReleaseCandidate

               //releaseStatusLastVersion = Some(ReleaseVirgin)
               releaseStatusPotentialRelease = Some(ReleaseVirgin)
               tagReleaseVirgin(commitIdPotentialRelease.get)
               if( commitIdPotentialRelease.get == latestRevCommit.get.name )
               {  log("   It is the latest version which has become ReleaseVirgin...")
                  releaseStatusLastVersion = Some(ReleaseVirgin)
               }
            }
            else
            {  log("   No, not possible. There is already a first release, and !latestReleaseHasSufficientSampleSize.")
            }
         }
         case _                     => { log("   There is no ReleaseCandidate, so not possible."); doNothing }
      }
   }

   /** Eliminates the role release candidate from the version which is the release candidate (there can only be one) of this constitution.
     */
   def unmakeCurrentPotentialRelease
   {  // search version which is now the potential release
      log("unmakeCurrentPotentialRelease")

      releaseStatusPotentialRelease match
      {  case Some(ReleaseCandidate) =>
         {  delTagReleaseCandidate
            commitIdPotentialRelease = None
            releaseStatusPotentialRelease = None
            if(releaseStatusLastVersion != Release) releaseStatusLastVersion = None // [COULDDO] &y2013.05.13.14:41:19& more elegant to automatically change this one as soon as releaseStatusPotentialRelease is changed with an "intelligent" setter.
         }
         case Some(ReleaseVirgin) =>
         {  delTagReleaseVirgin
            commitIdPotentialRelease = None
            releaseStatusPotentialRelease = None
            releaseStatusLastVersion = None
            if(releaseStatusLastVersion != Release) releaseStatusLastVersion = None // [COULDDO] see &y2013.05.13.14:41:19&
         }
         case None =>
         {  log("   [POTENTIAL_BUG] Wasn't a potential release anyway, dudicon!")
            log("   still attempting to erase all release info on this version...")
            delTagReleaseCandidate
            delTagReleaseVirgin
            commitIdPotentialRelease = None
            releaseStatusLastVersion = None
            releaseStatusPotentialRelease = None
            if(releaseStatusLastVersion != Release) releaseStatusLastVersion = None // [COULDDO] see &y2013.05.13.14:41:19&
         }
         case other     => logAndThrow("The following status shouldn't occur: " + other)
      }
      log(" TODO set wasReleaseCandidate tag as described in logs.")
   }

   /** Method should be called as soon as a player chooses this consti as his..her first constitution. If there is a release virgin, it is turned into a release.
     */
   def chosenAsFirstConsti =
   {  log("chosenAsFirstConsti called")
      log("   currentTime = " + timeInMillis2dateString(SystemWithTesting.currentTimeMillis))
      log("   consti = " + this.constiId)
      log("   releaseStatusPotentialRelease = " + releaseStatusPotentialRelease)
      if( releaseStatusPotentialRelease == Some(ReleaseVirgin) )
      {  log("   last version is ReleaseVirgin, so turning into Release.")

         val ci = commitIdPotentialRelease.get

         // remove virgin state
         delTagReleaseVirgin
         commitIdPotentialRelease = None
         releaseStatusPotentialRelease = None

         // add release state
         tagRelease(ci) // this one before adding it to commitIdsReleases, otherwise index naming will go wrong.
         commitIdsReleases ::= ci
      }
   }
}

class FollowerConsti_join extends LongKeyedMapper[FollowerConsti_join] with IdPK
{  def getSingleton = FollowerConsti_join
   object player extends MappedLongForeignKey(this, Player)
   object constiId extends MappedInt(this)
}

object FollowerConsti_join extends FollowerConsti_join with LongKeyedMetaMapper[FollowerConsti_join]
{  def join(player:Player, constiId:ConstiId)
   {  this.create.player(player).constiId(constiId)
   }
}

/* not yet used: for future increment
object ConstitutionMetaMapperObj extends Constitution(0, 0, 0, 0, "", None, Nil) with LongKeyedMetaMapper[Constitution]
{
}
*/

/** 
  * @param constiId constitution id, must be a number between 1 and this.count. Constitutions started being numbered from id = 1, and then without skipping any natural number up to the highestId.

  */
object Constitution
{  var constis:List[Constitution] = Nil
   var highestId:Int = 0 // <&y2012.03.18.17:31:11& deserialise in future when starting session>
   /*
   def create():Constitution =
   {  // if no user is given assume 'master' user, TODO <&y2012.05.24.20:44:12& determine fixed number for this, for now I used ID 0>
      create(0)
   }
   */

   def count =
   {  constis.size
   }

   def deserialize =
   {  log("Constitution.deserialize called")
      log("[MUSTDO] restore releasecandidate and releasevirgin tags in the right way from the git database.")
      val constObjDir = new File(GlobalConstant.CONSTITUTIONOBJECTDIR)
      val fileFilter:FileFilter = new WildcardFileFilter("Constitution*")
      val constitutionFiles = constObjDir.listFiles(fileFilter)
      if( constitutionFiles != null && constitutionFiles.length != 0 )
      {  implicit val formats = Serialization.formats(NoTypeHints) // <? &y2012.01.10.20:11:00& is this a 'closure' in action? It is namely used in the following function>
         // retrieve all tags needed for reconstructing releases.
         val taglist:scala.collection.mutable.Buffer[Ref] = scala.collection.JavaConversions.asBuffer(jgit.get.tagList.call)
         log("   names of tags found:")
         taglist.map( ref => log(ref.getName) )
 
         def readConst(file:File):Unit =
         {  log("   converting to Constitution-object: " + file.getPath )
            val in:BufferedReader   = new BufferedReader(new FileReader(file))
            var inStr:String        = in.readLine
            log("   serialized form: " + inStr)
            val const:Constitution  = Serialization.read[Constitution](inStr)
            constis ::= const
            // reconstruct releases from git repo
            def tagPointsToReleaseOf(tag:Ref, consti:Constitution):Boolean =
            {  val regex = new Regex("consti" + consti.constiId + ".release[0-9]+")
               val m     = regex.findFirstMatchIn(tag.getName)               
               m.isDefined
            }
            
            val tagsPointingToReleasesOfThisConsti = taglist.filter( tag => tagPointsToReleaseOf(tag, const) )
            const.commitIdsReleases = tagsPointingToReleasesOfThisConsti.map( tag => jgitRepo.get.peel(tag).getPeeledObjectId.name ).reverse.toList
            //const.commitIdsReleases = tagsPointingToReleasesOfThisConsti.map( tag => tag.getTarget.getPeeledObjectId.name ).reverse.toList
            log("   commitIdsReleases just read from git repo:")
            const.commitIdsReleases.map( log(_) )
         }
         
         constitutionFiles map readConst
         constis = constis.sortWith((c1,c2) => c1.constiId < c2.constiId )
         val highestIdFile       = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/highestId")
         val in:BufferedReader   = new BufferedReader(new FileReader(highestIdFile))
         var inStr:String        = in.readLine()
         highestId               = inStr.toInt
         log("   read from file: highestId = " + highestId)
         in.close
      }
      else
      {  log("   No serialised Constitution objects found in permanent storage! Lets start the day completely empty... Like a Zen buddhist.")
      }
   }

   def serialize = 
   {  log("object Constitution.serialize called")
      constis.map(_.serialize)

      var outFile = new File( GlobalConstant.CONSTITUTIONOBJECTDIR + "/highestId")
      log("   creating file: " + outFile.getAbsolutePath)
      // outFile.getParentFile().mkdirs() these should already exist
      // outFile.createNewFile() // <&y2011.12.23.13:39:00& is this required, or is the file automatically created when trying to write to it?>
      val out:PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter(outFile)))
      out.println(highestId.toString)
      out.close
   }

   def templateNewConstitution(constitutionId:Int):String =
"""<h2>Article 1</h2>

<p>...</p>
"""
   /** This method is not used for deserialisation purposes, solely for creating really new consti
     */
   def create(creatorUserID:Long):Constitution = 
   {  log("Constitution(Singleton Object).create called")
      log("   creatorUserID = " + creatorUserID)
      highestId += 1
      val now = if( !TestSettings.SIMULATEPLAYINGWITHJARARUNNING )
                  currentTimeMillis.toLong
                else
                  SystemWithTesting.currentTimeMillis

      val c = Constitution( highestId, now, creatorUserID, 0, "No description provided.", None, List(creatorUserID), List(creatorUserID), None, None, None)
      constis = c::constis
      c
   }

   def getById(constiId:Int):Option[Constitution] =
   {  constis.find( _.constiId == constiId )
   }

   def remove(c:Constitution) = 
   {  constis = constis.filterNot( _ == c )
   }

   def removeAll =
   {  constis = Nil
      highestId = 0
   }

   /** @todo Optimize
     */
   def constisWithAReleaseOrVirginRelease:List[Constitution] =
   {  constis.filter
      {  c => 
         { c.firstReleaseExists || c.releaseStatusPotentialRelease == Some(ReleaseVirgin)
         }
      }
   }   

   def constisWithTrailingVersionsWithoutReleaseStatus:List[Constitution] =
   {  constis.filter
      {  c => 
         { c.releaseStatusLastVersion == None
         }
      }
   }

   def createConstiAlphaIfDoesntExist =
   {  if(constis == Nil)
      {  log("There are no constitutions yet, so adding constitution alpha to constitution-population.")
         val constiAlphaStr = scala.io.Source.fromFile(GlobalConstant.CONSTiALPHaINIT).mkString
         val adminId = GlobalConstant.adminOpt.get.id.is
         val constiAlpha = Constitution.create(adminId)
         constiAlpha.publish( constiAlphaStr, "first publication", adminId.toString )
         constiAlpha.makeLatestVersionReleaseCandidateIfPossible
      }
   }
}

class StudyHistory
{  private var constitutionStudyHistories:List[ConstitutionStudyHistory] = Nil

   /* OUT: false: not successful (interval overlapping another one)
   */
   def addInterval(consti:Constitution, i:TimeInterval):Boolean =
   {  // TODO next milestone not now
      true
   }

   def getIntervals(consti:Constitution):List[TimeInterval] =
   {  // TODO next milestone not now

      Nil
   }
}

// TODO implement for next increment
class ConstitutionStudyHistory(val consti:Constitution)
{  var studyTimeIntervals:List[TimeInterval] = Nil
}

// TODO implement for next increment
case class TimeInterval(val startTime:Long, val endTime:Long)

object OCBKCinfoPlayer
{
   /** @todo coulddo: optimise by caching/memoization/incremental update of cached value?
     @ @todo move to gamecore library, because this is not a OCBKC specific method.
     */
   def numberOfSessionsPlayedBy(p:Player) =
   {  val ccs:List[CoreContent] = PlayerCoreContent_join.findAll( By(PlayerCoreContent_join.player, p) ).map( join => join.coreContent.obj.open_! )
      ccs.size
   }
   
   /**
     */
   def playerHasAccessToAllConstis(p:Player) =
   {  numberOfSessionsPlayedBy(p) >= OneToStartWith.minSessionsB4access2allConstis
   }
}

package scoring
{
import GlobalConstant.AverageFluency

object PlayerScores
{  // TODO: build in optimizations by caching calculation results in local variables of this object. But first find out whether the object is shared among user threads, otherwise these intermediate calculations cannot be shared among players. Perhaps better store them in the database instead of local variables?
   case class Result_percentageCorrect(val percentageCorrect:Option[Double], val totalNumberOfSessions:Int)

   // <&y2012.11.11.16:05:26& TODO: move to more general lib?>
   def takeNumOrAll[A](list:List[A], num:Int) =
   {  if( num > -1 )
         list.take(num)
      else
         list
   }

   def percentageCorrect(p:Player):Result_percentageCorrect = 
   {  percentageCorrect(p, -1)
   }

   /** @param numOfSessions only the first numOfSessions of sessions played by the Player will be part of the calculation. If -1 is provided, ALL sessions will be part of it.
     * 
     */
   def percentageCorrect(p:Player, numOfSessions:Int):Result_percentageCorrect = 
   {  log("percentageCorrect called")

      val ccs:List[CoreContent] = takeNumOrAll(PlayerCoreContent_join.findAll( By(PlayerCoreContent_join.player, p) ).map( join => join.coreContent.obj.open_! ).sortWith{ (cc1, cc2)  => cc1.startTime.get < cc2.startTime.get }, numOfSessions)


      //val correctCcs = ccs.filter( cc => cc.answerPlayerCorrect )
      val numberCorrect = ccs.count( cc => cc.answerPlayerCorrect )
      val totalNumber = ccs.length

      log("   Number of sessions taken into consideration: " + totalNumber)
      log("   Datetimes of sessions taken into consideration: " + ccs.map(cc => cc.startTime.get))

      val percCorrect = if( totalNumber != 0) Some(numberCorrect.toDouble/totalNumber.toDouble * 100.0) else None
      Result_percentageCorrect(percCorrect, totalNumber)
   }

   case class Result_averageDurationTranslation(val averageDurationTranslation:Option[Double], val sampleSize: Int)
/** @return only includes time of correct translations
  */
   def averageDurationTranslation(p:Player):Result_averageDurationTranslation =
   {  averageDurationTranslation(p, -1)
   }

/** @param numOfSessions only the first numOfSessions of sessions played by the Player will be part of the calculation. If -1 is provided, ALL sessions will be part of it.
  * @return only includes times of correct translations. Note that totalNumOfSessionsWithCorrectTranslations only counts the correct sessions within the numOfSessions first sessions.
  */
   def averageDurationTranslation(p:Player, numOfSessions:Int):Result_averageDurationTranslation = 
   {  log("PlayerScores.averageDurationTranslation called")
      val ccs:List[CoreContent] = takeNumOrAll(PlayerCoreContent_join.findAll( By(PlayerCoreContent_join.player, p) ).map( join => join.coreContent.obj.open_! ).sortWith{ (cc1, cc2)  => cc1.startTime.get < cc2.startTime.get }, numOfSessions)
      
      val correctCcs = ccs.filter( cc => cc.answerPlayerCorrect )
      val durationsCorrectTranslations = correctCcs.map(cc => cc.durationTranslation.get)
      val numberCorrect = correctCcs.length
      log("   numberCorrect = " + numberCorrect )
      val averageDurationTranslation = if( numberCorrect > 0 ) Some(( durationsCorrectTranslations.fold(0L)(_ + _).toDouble )) else None
      Result_averageDurationTranslation(averageDurationTranslation, numberCorrect)
   }

   /*
    // <&y2012.10.06.11:13:23& refactor PlayerStats using this instead?>
   case class PlayerStats(sessionWithShortestDurationAndCorrectTranslation,  
   {  
   }
   */

}
/* In a separate object instead of as a methods of class Constitutions, because some scores might be relative (e.g. a ranking), this constitution is better than that one.
*/

object ConstiScores
/* <&y2012.10.06.19:40:31& add some confidence measure to this, for example average sample size>
*/
{/**
  * @return The average percentage correct for the last release of this constitution (so not the average over all releases!).
  */
   def averagePercentageCorrect(minimalNumberOfSessionsPerPlayer:Int, constiId:ConstiId):Option[Double] =
   {  log("averagePercentageCorrect called")
      Constitution.getById(constiId) match
      {  case Some(consti) => 
            consti.lastReleaseCommitId match
            {  case Some(lastReleaseCommitId) =>  averagePercentageCorrect(minimalNumberOfSessionsPerPlayer, lastReleaseCommitId)
               case None => None
            }
         case None => None
      }
   }

/**
  * @return The average percentage correct for the last release of this constitution (so not the average over all releases!).
  */
   def averagePercentageCorrect(minimalNumberOfSessionsPerPlayer:Int, releaseId:String):Option[Double] =
   {  log("averagePercentageCorrect called")
      if(minimalNumberOfSessionsPerPlayer > OneToStartWith.minSessionsB4access2allConstis) throw new RuntimeException("   minimalNumberOfSessionsPerPlayer > OneToStartWith.minSessionsB4access2allConstis, condition can never be satisfied. If I were you, I would change either of two such that it CAN be satisfied, my friend")
      val players = Player.findAll
      // choose player: with first chosen constitution = consti with constiId, however, you must also be certain that they didn't play SO long that influences of e.g. other constitutions started to play a role!
      val playersWithThisRelease:List[Player] = players.filter(
         p => ( p.releaseOfFirstChosenConstitution.get == releaseId )
      )

      log("   playersWithThisRelease:" + playersWithThisRelease)
      
      val percentages:List[Double] =
      playersWithThisRelease.map(
         player => 
         {  val res = PlayerScores.percentageCorrect(player, OneToStartWith.minSessionsB4access2allConstis) // TODO only count sessions < minSesionsB4access2allConstis
            if( res.totalNumberOfSessions >= minimalNumberOfSessionsPerPlayer )
            {  res.percentageCorrect // note: will also be None when the player didn't play any sessions yet
            }
            else
            {  None            
            }
         } 
      ).collect{ case Some(p) => p }

      log("   percentages: " + percentages)

      def add(p1:Double, p2:Double) = p1 + p2
      val avPercCor = if(percentages.isEmpty) None else Some((percentages.fold(0d)(add))/percentages.size)
      
      avPercCor
   }
   // COULDDO: refactor, extract generalities from averageDurationTranslation and averagePercentageCorrect
   def averageDurationTranslation(minimalNumberOfSessionsPerPlayer:Int, constiId:ConstiId):Option[Double] = 
   {  Constitution.getById(constiId) match
      {  case Some(consti) => 
            consti.lastReleaseCommitId match
            {  case Some(lastReleaseCommitId) => averageDurationTranslation(minimalNumberOfSessionsPerPlayer, lastReleaseCommitId)
               case None => None
            }
         case None => None
      }
   }
   /* marked for deletion

   object lastReleaseWithFluencyScore
   {  def apply
      
      def lastReleaseWithFluencyScore_Whisper_OFW(constiId:ConstiId, releaseId:Option[String]) =
   {  // TODO &y2013.01.21.18:55:52& note: double Option in cache: if None => value is unknown (not cached), or Some(None) => value is known: there is NO release with score...
   }

   def lastReleaseWithFluencyScore_Pure_OFW(constiId: ConstiId):Option[VersionId] =
   {     }

   def lastReleaseWithFluencyScore(constiId:ConstiId) =
   {  // TODO implement optimization
      lastReleaseWithFluencyScore_Pure_OFW(constiId)
   }
   */
   def averageDurationTranslation(minimalNumberOfSessionsPerPlayer:Int, releaseId:String):Option[Double] = 
   {  log("ConstiScores.averageDurationTranslation called")
      if(minimalNumberOfSessionsPerPlayer > OneToStartWith.minSessionsB4access2allConstis) throw new RuntimeException("   minimalNumberOfSessionsPerPlayer > OneToStartWith.minSesionsB4access2allConstis, condition can never be satisfied. If I were you, I would change either of two such that it CAN be satisfied, my friend")
      val players = Player.findAll
      // choose player: with first chosen constitution = consti with constiId, however, you must also be certain that they didn't play SO long that influences of e.g. other constitutions started to play a role!
      val playersWithThisRelease:List[Player] = players.filter(
         p => ( p.releaseOfFirstChosenConstitution.get == releaseId )
      )

      log("   playersWithThisRelease:" + playersWithThisRelease)
      
      val averageDurationTranslationPerPlayer:List[Double] =
      playersWithThisRelease.map(
         player => 
         {  val res = PlayerScores.averageDurationTranslation(player, OneToStartWith.minSessionsB4access2allConstis) // TODO only count sessions < minSesionsB4access2allConstis
            if( res.sampleSize >= minimalNumberOfSessionsPerPlayer ) // COULDDO perhaps rename minimalNumberOfSessionsPerPlayer also to minimalSampleSize
            {  res.averageDurationTranslation // note: will also be None when the player didn't play any sessions yet
            }
            else
            {  None            
            }
         } 
      ).collect{ case Some(p) => p }

      log("   averageDurationTranslations: " + averageDurationTranslationPerPlayer)

      def add(p1:Double, p2:Double) = p1 + p2
      val averageDurationTranslations = if(averageDurationTranslationPerPlayer.isEmpty) None else Some((averageDurationTranslationPerPlayer.fold(0d)(add))/averageDurationTranslationPerPlayer.size)
      
      averageDurationTranslations
   }

   def applyWhenBothDefined[A,B,C](g:(A,B) => C, a:Option[A], b:Option[B]):Option[C] =
   {  if( a == None || b == None )
         None
      else
         Some(g(a.get, b.get))
   }

   /**  Find first element 
     *  @todo move to more generic lib.

     * Note that List.collectFirst does something different:  val list = List.range(0,15); list.collectFirst{ x:Int => x match { case _ if x < 4 => None; case _ if x >= 4 => Some(x); } } will yield Some(None). This function, however, will interpret the result None as not found and will continue to search.
     */
   def findAndApply[A,B](list:List[A], f:A => Option[B]):Option[B] =
   {  var result:Option[Option[B]] = None
      list.find
      {  a =>
         {  result = Some(f(a))
            result.get.isDefined
         }
      }
      match
      {  case None => None
         case Some(a) => result.get
      }
   }

   /* <&y2013.02.10.17:22:46& mustdo OPTIMISE by memoizatoin/caching>
   */
   def latestReleaseWithFluencyScore(constiId:ConstiId):Option[VersionId] = 
   {  averageFluencyLatestReleaseWithScore(AverageFluency.minimalSampleSizePerPlayer, constiId, AverageFluency.fluencyConstantK).collect{ case v_f:(VersionId,Double) => v_f._1 }
   }

   /** @param constiId an id of an existing constitution. If it doesn't exist, this is considered a bug, and, moreover, an exception is thrown!
     * @return The fluency score of the latest release with a fluency score.
     * @todo &y2013.02.10.17:24:46& optimise (also see latestReleaseWithScore), suggestion: merge these two methods: the core being this method, the other calling this method, and retrieving a cached value.
     * @todo &y2013.02.10.18:51:54& get minimalSampleSize and k from global
     */

   def averageFluencyLatestReleaseWithScore(minimalSampleSize: Int, constiId:ConstiId, k:Double):Option[(VersionId,Double)] =
   {  findAndApply( Constitution.getById(constiId).get.commitIdsReleases,
                    {   versionId:VersionId =>
                        {  averageFluency(minimalSampleSize, versionId, k).collect{ case f => (versionId, f) }
                        }
                    }
      )
   }

   // SHOULDDO &y2013.01.21.19:23:39&: directly retrieve the values minimalSampleSize and k from Global. For this purpose write a wrapper aroudn this function, so that it can still be called if you want to locally apply a deviating calculation.
   /** @todo  &y2013.01.07.13:20:00& is there a check whether releaseId is indeed a release (and not another version)?
     */
   def averageFluency(minimalSampleSize: Int, releaseId:String, k:Double):Option[Double] =
   {  val adt = averageDurationTranslation(minimalSampleSize, releaseId) 
      val apc = averagePercentageCorrect(minimalSampleSize, releaseId)
      applyWhenBothDefined( (_:Double)/(_:Double)*k, apc, adt)
   }


   // perhaps not needed anymore, found an "absolute" score for fluency
   def fluency1stGT2nd(releaseId1:String, releaseId2:String, minimalSampleSize: Int):Option[Boolean] =
   {  val adt1 = averageDurationTranslation(minimalSampleSize, releaseId1) 
      val adt2 = averageDurationTranslation(minimalSampleSize, releaseId2)

      val ratio_adt1adt2 = applyWhenBothDefined( (_:Double)/(_:Double), adt1, adt2)

      val apc1 = averagePercentageCorrect(minimalSampleSize, releaseId1)
      val apc2 = averagePercentageCorrect(minimalSampleSize, releaseId2)

      val ratio_apc1apc2 = applyWhenBothDefined( (_:Double)/(_:Double), apc1, apc2)
      /*
         if( apc1 == None || apc2 == None )
            None
         else
            Some(apc1.get/apc2.get) // TODO: division by zero error.
      
      if( ratio_adt1adt2 == None || ratio_apc1apc2 == None )
      {  None
      } else
      {  Some(ratio_adt1adt2.get > ratio_apc1apc2.get)
      } */
      applyWhenBothDefined( (_:Double) > (_:Double), ratio_adt1adt2, ratio_apc1apc2)
   }

   /** Helpfunction: returns the total number of sessions played by a player, the number of correct sessions, and the average playing time per correct session. 
     *
    **/
/* [&y2012.10.08.17:42:22& still needed?]
   def firstN(c:Constitution, N: Int, sps:ScorePerSession) =
   {  //TODO
      val firstNperPlayerList = TODOgetAllPlayers.map(firstNplayer(c, N, sps, _))
      firstNperPlayerList.sum / firstNperPlayerList.length
   }

   <<< EUC */

   def inversePlayingTimeIfCorrect(c:Constitution /* TODO */) =
   {  //TODO
   }

   def sampleSizeSufficient4FluencyScore(releaseId:String):Boolean =
   {  // the sample size is sufficiently large, if the fluency score exists.
      val isSuf = averageFluency(GlobalConstant.AverageFluency.minimalSampleSizePerPlayer, releaseId, GlobalConstant.AverageFluency.fluencyConstantK).isDefined
      isSuf
   }

   /** Determine whether sample size of latest release is sufficient for scoring
      * @todo &y2013.01.23.13:59:27& investigate whether this is really optimal in this way. Isn't this function called too often? (I don't think so, but I'm not sure).
        @return None, if there is no release yet of this constitution
     */
   def sampleSizeSufficient4FluencyScore(constiId:ConstiId):Option[Boolean] =
   {  // the sample size is sufficiently large, if the fluency score exists.
      Constitution.getById(constiId) match
      {  case Some(consti) => 
            consti.lastReleaseCommitId match
            {  case Some(lastReleaseCommitId) =>
               {  Some(sampleSizeSufficient4FluencyScore(lastReleaseCommitId))
               }
               case None => None
            }
         case None => None
      }
   }

/* &y2012.08.15.11:17:14& Elegant way, but perhaps too complicated (= inefficient in execution) for the problem
   val inversePlayingTimeIfCorrect =
      new ScorePerSession
      {  def apply(c:Constitution, ) =
         {  
         }
      }
*/
}
/* idea put on hold for now:
abstract class ScorePerSession // TODO extends ( TODOin => TODOout )
{  // unspecified method defining types of apply
   // TODO def apply(in:TODOin):TODOout
}
*/
}

abstract class ConstiSelectionProcedure
case object NoProc extends ConstiSelectionProcedure
case object OneToStartWith extends ConstiSelectionProcedure
{  val minSessionsB4access2allConstis = GlobalConstant.MINsESSIONSb4ACCESS2ALLcONSTIS
}
}
