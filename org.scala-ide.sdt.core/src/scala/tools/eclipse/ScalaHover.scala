/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.eclipse 

import org.eclipse.jdt.core.{IJavaElement, ICodeAssist}
import org.eclipse.jdt.core._
import org.eclipse.jdt.internal.ui.text.java.hover.JavadocHover
import org.eclipse.jface.text.{ITextViewer, IRegion}
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import scala.tools.nsc.symtab.Flags
import org.eclipse.jdt.internal.core.JavaModelManager
import scala.tools.eclipse.contribution.weaving.jdt.IScalaElement
import scala.tools.eclipse.javaelements.ScalaSourceTypeElement

class ScalaHover(codeAssist: Option[ICodeAssist]) extends JavadocHover {
  
  private val NoHoverInfo = "" // could return null, but prefer to return empty (see API of ITextHover).

  override def getJavaElementsAt(textViewer: ITextViewer, region: IRegion) = {
    codeAssist match {
      case Some(scu: ScalaCompilationUnit) => {
        val start = region.getOffset
        val end = start + region.getLength
        scu.withSourceFile({ (src, compiler) =>
          import compiler._
          def hoverInfo(t: Tree): Option[IMember] = askOption { () =>
            val element = compiler.getJavaElement(t.symbol)
            if (element.nonEmpty) {
              element.get
            } else {
              null
            }
          }
          val resp = new Response[Tree]
          val range = compiler.rangePos(src, start, start, end)
          askTypeAt(range, resp)
          var element: Option[IMember] = None
          if (resp.get.isLeft) {
            element = hoverInfo(resp.get.left.get)
          }
          if (element.nonEmpty && (!element.isInstanceOf[IScalaElement])) {
            Array[IJavaElement](element.get)
          } else {
            Array[IJavaElement]()
          }
        })(Array[IJavaElement]())
      }
      case _ => Array[IJavaElement]()
    }
  }

  override def getHoverInfo2(textViewer: ITextViewer, hoverRegion: IRegion): Object = {
    val javaElements = getJavaElementsAt(textViewer, hoverRegion);
    if (javaElements.length > 0 && !javaElements(0).isInstanceOf[ScalaSourceTypeElement]) {
      try {
        super.getHoverInfo2(textViewer, hoverRegion)
      } catch {
        case e => getHoverInfo(textViewer, hoverRegion)
      }
    } else {
      getHoverInfo(textViewer, hoverRegion)
    }
  }

  override def getHoverInfo(viewer: ITextViewer, region: IRegion) = {
    codeAssist match {
      case Some(scu: ScalaCompilationUnit) => {
        val start = region.getOffset
        val end = start + region.getLength
        scu.withSourceFile({ (src, compiler) =>
          import compiler._
          def hoverInfo(t: Tree): Option[String] = askOption { () =>
            def compose(ss: List[String]): String = ss.filter("" !=).mkString("", " ", "")
            def defString(sym: Symbol, tpe: Type): String = {
              // NoType is returned for defining occurrences, in this case we want to display symbol info itself. 
              val tpeinfo = if (tpe ne NoType) tpe.widen else sym.info
              compose(List(sym.hasFlagsToString(Flags.ExplicitFlags), sym.keyString,
                sym.varianceString + sym.nameString + sym.infoString(tpeinfo)))
            }
            
            for (sym <- Option(t.symbol); tpe <- Option(t.tpe)) 
              yield {
               val com = rawDocComment(sym) 
               val doc = toJavaDoc(rawDocComment(sym));
                if (!doc.equals("")) doc
                else if (sym.isClass || sym.isModule) sym.fullName 
                else defString(sym, tpe)                     
              }                        
          } getOrElse None
          
          val resp = new Response[Tree]
          val range = compiler.rangePos(src, start, start, end)
          askTypeAt(range, resp)
          (for (t <- resp.get.left.toOption; hover <- hoverInfo(t)) yield hover) getOrElse NoHoverInfo
        })(NoHoverInfo)
      }
      case _ => NoHoverInfo
    }
  }

  override def getHoverRegion(viewer: ITextViewer, offset: Int) = {
    ScalaWordFinder.findWord(viewer.getDocument, offset)
  }
}
