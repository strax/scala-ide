/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.eclipse
package javaelements

import java.util.{ Map => JMap }

import scala.concurrent.SyncVar

import org.eclipse.core.internal.filebuffers.SynchronizableDocument
import org.eclipse.core.resources.{ IFile, IResource }
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.{
  BufferChangedEvent, CompletionRequestor, IBuffer, IBufferChangedListener, IJavaElement, IJavaModelStatusConstants,
  IProblemRequestor, ITypeRoot, JavaCore, JavaModelException, WorkingCopyOwner, IClassFile }
import org.eclipse.jdt.internal.compiler.env
import org.eclipse.jdt.internal.core.{
  BufferManager, CompilationUnitElementInfo, DefaultWorkingCopyOwner, JavaModelStatus, JavaProject, Openable,
  OpenableElementInfo, SearchableEnvironment }
import org.eclipse.jdt.internal.core.search.matching.{ MatchLocator, PossibleMatch }
import org.eclipse.jdt.internal.ui.javaeditor.DocumentAdapter
import org.eclipse.jface.text.{IRegion, ITextSelection}
import org.eclipse.ui.texteditor.ITextEditor 

import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.util.{ BatchSourceFile, SourceFile }

import scala.tools.eclipse.contribution.weaving.jdt.{ IScalaCompilationUnit, IScalaWordFinder }

import scala.tools.eclipse.{ ScalaImages, ScalaPlugin, ScalaPresentationCompiler, ScalaSourceIndexer, ScalaWordFinder }
import scala.tools.eclipse.util.ReflectionUtils

trait ScalaCompilationUnit extends Openable with env.ICompilationUnit with ScalaElement 
     with IScalaCompilationUnit with IBufferChangedListener with JavadocUtils {
  
  val project = ScalaPlugin.plugin.getScalaProject(getJavaProject.getProject)

  val file : AbstractFile

  def doWithSourceFile(op : (SourceFile, ScalaPresentationCompiler) => Unit) {
    project.withSourceFile(this)(op)(())
  }
  
  def withSourceFile[T](op : (SourceFile, ScalaPresentationCompiler) => T)(orElse: => T = project.defaultOrElse) : T = {
    project.withSourceFile(this)(op)(orElse)
  }
  
  override def bufferChanged(e : BufferChangedEvent) {
    if (!e.getBuffer.isClosed)
      project.doWithPresentationCompiler(_.askReload(this, getContents))

    super.bufferChanged(e)
  }
  
  def createSourceFile : BatchSourceFile = {
    new BatchSourceFile(file, getContents)
  }

  def getProblemRequestor : IProblemRequestor = null

  override def buildStructure(info : OpenableElementInfo, pm : IProgressMonitor, newElements : JMap[_, _], underlyingResource : IResource) : Boolean =
    withSourceFile({ (sourceFile, compiler) =>
      val unsafeElements = newElements.asInstanceOf[JMap[AnyRef, AnyRef]]
      val tmpMap = new java.util.HashMap[AnyRef, AnyRef]
      val sourceLength = sourceFile.length
      
      try {
        compiler.withStructure(sourceFile) { tree =>
          compiler.askOption { () =>
              new compiler.StructureBuilderTraverser(this, info, tmpMap, sourceLength).traverse(tree)
          }
        }
        info match {
          case cuei : CompilationUnitElementInfo => 
            cuei.setSourceLength(sourceLength)
          case _ =>
        }
    
        unsafeElements.putAll(tmpMap)
        info.setIsStructureKnown(true)
      } catch {
        case ex => 
          ScalaPlugin.plugin.logError("Error building structure for %s".format(sourceFile), ex)
          info.setIsStructureKnown(false)
      }
      info.isStructureKnown
    }) (false)

  def scheduleReconcile : Unit = ()
  
  def addToIndexer(indexer : ScalaSourceIndexer) {
    doWithSourceFile { (source, compiler) =>
      compiler.withParseTree(source) { tree =>
        new compiler.IndexBuilderTraverser(indexer).traverse(tree)
      }
    }
  }
  
  def newSearchableEnvironment(workingCopyOwner : WorkingCopyOwner) : SearchableEnvironment = {
    val javaProject = getJavaProject.asInstanceOf[JavaProject]
    javaProject.newSearchableNameEnvironment(workingCopyOwner)
  }

  def newSearchableEnvironment() : SearchableEnvironment =
    newSearchableEnvironment(DefaultWorkingCopyOwner.PRIMARY)
  
  override def getSourceElementAt(pos : Int) : IJavaElement = {
    super.getSourceElementAt(pos) match {
      case smie : ScalaModuleInstanceElement => smie.getParent;
      case elem => elem 
    }
  }
    
  override def codeSelect(cu : env.ICompilationUnit, offset : Int, length : Int, workingCopyOwner : WorkingCopyOwner) : Array[IJavaElement] = {
    withSourceFile({ (src, compiler) =>
      import compiler._
      
      //@Iulian & co: this method is really badly placed here since we update the 
      //              JavadocView as a side-effect of the 'codeSelect' 
      def updateJavadocView(sym : Symbol, tpe : Type) {
    	import org.eclipse.jdt.internal.ui._
    	import org.eclipse.jdt.internal.ui.text.java.hover._
    	import org.eclipse.jdt.internal.ui.infoviews._
        import org.eclipse.jdt.ui._
        import org.eclipse.jface.internal.text.html._
        import org.eclipse.ui._
        import scala.tools.eclipse.util._        
        
        SWTUtils.asyncExec {
    	    import org.eclipse.ui.internal._
	        val window = PlatformUI.getWorkbench.getActiveWorkbenchWindow().getActivePage().asInstanceOf[WorkbenchPage];
	        val perspective = window.getActivePerspective();
	        val viewRef = perspective.findView(JavaUI.ID_JAVADOC_VIEW, null);
	        if (viewRef == null)
	          return;
	        val view = viewRef.getView(true).asInstanceOf[JavadocView];	    
	    	val buffer= new StringBuffer();    
	        HTMLPrinter.insertPageProlog(buffer, 0, styleSheet);
	        val html = buildCommentAsHtml(this, sym, tpe)    	
    	    buffer.append(html)
	        HTMLPrinter.addPageEpilog(buffer);
	        
	        val javadocViewClazz = Class.forName("org.eclipse.jdt.internal.ui.infoviews.JavadocView")
	        val objectClazz = Class.forName("java.lang.Object")
	        val doSetInputMethod = getDeclaredMethod(javadocViewClazz, "doSetInput", objectClazz); 	        
	        doSetInputMethod.invoke(view, buffer.toString);
    	}
      }
      
      def computeJavaElement(t: Tree) : Option[IJavaElement] = { 
        askOption { () =>
          import scala.reflect.internal._
          if (t.symbol != null && t.tpe != null && !t.symbol.hasFlag(Flags.JAVA)) { 
        	//if the symbol comes from scala, then update the JavadocView
            updateJavadocView(t.symbol, t.tpe)
          	None
          } else //for Java symbols, leave the JDT do the work of updating the JavadocView
            Option(t.symbol).flatMap(getJavaElement2(_))
        }.flatten     
      }
      
      val resp = new Response[Tree]
      val range = rangePos(src, offset, offset, offset + length)
      askTypeAt(range, resp)
      val r = resp.get.left.toOption.map(computeJavaElement(_)).flatten
      r.map(Array(_)).getOrElse(Array.empty[IJavaElement])      
    })(Array.empty[IJavaElement])        
  }

  def codeComplete
    (cu : env.ICompilationUnit, unitToSkip : env.ICompilationUnit,
     position : Int,  requestor : CompletionRequestor, owner : WorkingCopyOwner, typeRoot : ITypeRoot) {
     codeComplete(cu, unitToSkip, position, requestor, owner, typeRoot, null) 
  }
    
  override def codeComplete
    (cu : env.ICompilationUnit, unitToSkip : env.ICompilationUnit,
     position : Int,  requestor : CompletionRequestor, owner : WorkingCopyOwner, typeRoot : ITypeRoot,
     monitor : IProgressMonitor) {
  }
  
  override def reportMatches(matchLocator : MatchLocator, possibleMatch : PossibleMatch) {
    doWithSourceFile { (sourceFile, compiler) =>
      compiler.withStructure(sourceFile) { tree =>
        compiler.askOption { () =>
          compiler.MatchLocator(this, matchLocator, possibleMatch).traverse(tree)
        }
      }
    }
  }
  
  override def createOverrideIndicators(annotationMap : JMap[_, _]) {
    doWithSourceFile { (sourceFile, compiler) =>
      compiler.withStructure(sourceFile) { tree =>
        compiler.askOption { () =>
          new compiler.OverrideIndicatorBuilderTraverser(this, annotationMap.asInstanceOf[JMap[AnyRef, AnyRef]]).traverse(tree)
        }
      }
    }
  }
  
  override def getImageDescriptor = {
    Option(getCorrespondingResource) map { file =>
      import ScalaImages.{ SCALA_FILE, EXCLUDED_SCALA_FILE }
      val javaProject = JavaCore.create(project.underlying)
      if (javaProject.isOnClasspath(file)) SCALA_FILE else EXCLUDED_SCALA_FILE
    } orNull
  }
  
  override def getScalaWordFinder() : IScalaWordFinder = ScalaWordFinder
  
  def followReference(editor : ITextEditor, selection : ITextSelection) = {
    val region = new IRegion {
      def getOffset = selection.getOffset
      def getLength = selection.getLength
    }
    new ScalaHyperlinkDetector().detectHyperlinks(editor, region, false) match {
      case Array(hyp) => hyp.open
      case _ =>  
    }
  }
}

object OpenableUtils extends ReflectionUtils {
  private val oClazz = classOf[Openable]
  private val openBufferMethod = getDeclaredMethod(oClazz, "openBuffer", classOf[IProgressMonitor], classOf[AnyRef])
  private val getBufferManagerMethod = getDeclaredMethod(oClazz, "getBufferManager")

  def openBuffer(o : Openable, pm : IProgressMonitor, info : AnyRef) : IBuffer = openBufferMethod.invoke(o, pm, info).asInstanceOf[IBuffer]
  def getBufferManager(o : Openable) : BufferManager = getBufferManagerMethod.invoke(o).asInstanceOf[BufferManager]
}
