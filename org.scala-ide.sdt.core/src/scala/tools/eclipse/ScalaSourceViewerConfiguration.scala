/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.eclipse;

import org.eclipse.jface.text.formatter.MultiPassContentFormatter
import org.eclipse.jface.util.PropertyChangeEvent
import org.eclipse.jdt.core.{ IJavaProject, IJavaElement, ICodeAssist }
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.internal.ui.javaeditor.{ IClassFileEditorInput, ICompilationUnitDocumentProvider, JavaElementHyperlinkDetector }
import org.eclipse.jdt.internal.ui.text.ContentAssistPreference
import org.eclipse.jdt.internal.ui.text.java.{ JavaAutoIndentStrategy, JavaStringAutoIndentStrategy, SmartSemicolonAutoEditStrategy }
import org.eclipse.jdt.internal.ui.text.java.hover.{ AbstractJavaEditorTextHover, BestMatchHover }
import org.eclipse.jdt.internal.ui.text.javadoc.JavaDocAutoIndentStrategy
import org.eclipse.jdt.ui.text.{ JavaSourceViewerConfiguration, IJavaPartitions }
import org.eclipse.jface.preference.IPreferenceStore
import org.eclipse.jface.text.{ IAutoEditStrategy, IDocument, ITextHover }
import org.eclipse.jface.text.formatter.ContentFormatter
import org.eclipse.jface.text.contentassist.ContentAssistant
import org.eclipse.jface.text.contentassist.IContentAssistant
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector
import org.eclipse.jface.text.presentation.PresentationReconciler
import org.eclipse.jface.text.rules.{ DefaultDamagerRepairer, RuleBasedScanner, ITokenScanner }
import org.eclipse.jface.text.source.ISourceViewer
import org.eclipse.jface.util.PropertyChangeEvent
import org.eclipse.ui.texteditor.{ HyperlinkDetectorDescriptor, ITextEditor }
import org.eclipse.swt.SWT
import scala.tools.eclipse.ui.{ JdtPreferenceProvider, ScalaAutoIndentStrategy, ScalaIndenter }
import scala.tools.eclipse.util.ReflectionUtils
import scala.tools.eclipse.lexical._
import scala.tools.eclipse.formatter.ScalaFormattingStrategy
import scala.tools.eclipse.properties.ScalaSyntaxClasses
import org.eclipse.jface.text.information.IInformationPresenter
import org.eclipse.jdt.internal.ui.text.java.hover.JavaInformationProvider
import org.eclipse.jdt.internal.ui.text.java.hover.JavaTypeHover

class ScalaSourceViewerConfiguration(store: IPreferenceStore, scalaPreferenceStore: IPreferenceStore, editor: ITextEditor)
   extends JavaSourceViewerConfiguration(JavaPlugin.getDefault.getJavaTextTools.getColorManager, store, editor, IJavaPartitions.JAVA_PARTITIONING) 
   with ReflectionUtils {

   private val codeScanner = new ScalaCodeScanner(getColorManager, store)

   override def getPresentationReconciler(sv: ISourceViewer) = {
      val reconciler = super.getPresentationReconciler(sv).asInstanceOf[PresentationReconciler]
      val dr = new ScalaDamagerRepairer(codeScanner)

      reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE)
      reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE)

      def handlePartition(partitionType: String, tokenScanner: ITokenScanner) {
         val dr = new DefaultDamagerRepairer(tokenScanner)
         reconciler.setDamager(dr, partitionType)
         reconciler.setRepairer(dr, partitionType)
      }

      handlePartition(IDocument.DEFAULT_CONTENT_TYPE, scalaCodeScanner)
      handlePartition(IJavaPartitions.JAVA_DOC, scaladocScanner)
      handlePartition(IJavaPartitions.JAVA_SINGLE_LINE_COMMENT, singleLineCommentScanner)
      handlePartition(IJavaPartitions.JAVA_MULTI_LINE_COMMENT, multiLineCommentScanner)
      handlePartition(IJavaPartitions.JAVA_STRING, stringScanner)
      handlePartition(ScalaPartitions.SCALA_MULTI_LINE_STRING, multiLineStringScanner)
      handlePartition(ScalaPartitions.XML_TAG, xmlTagScanner)
      handlePartition(ScalaPartitions.XML_COMMENT, xmlCommentScanner)
      handlePartition(ScalaPartitions.XML_CDATA, xmlCDATAScanner)
      handlePartition(ScalaPartitions.XML_PCDATA, xmlPCDATAScanner)
      handlePartition(ScalaPartitions.XML_PI, xmlPIScanner)

      reconciler
   }

   private val scalaCodeScanner = new ScalaCodeScanner(getColorManager, scalaPreferenceStore)
   private val singleLineCommentScanner = new SingleTokenScanner(ScalaSyntaxClasses.SINGLE_LINE_COMMENT, getColorManager, scalaPreferenceStore)
   private val multiLineCommentScanner = new SingleTokenScanner(ScalaSyntaxClasses.MULTI_LINE_COMMENT, getColorManager, scalaPreferenceStore)
   private val scaladocScanner = new SingleTokenScanner(ScalaSyntaxClasses.SCALADOC, getColorManager, scalaPreferenceStore)
   private val stringScanner = new SingleTokenScanner(ScalaSyntaxClasses.STRING, getColorManager, scalaPreferenceStore)
   private val multiLineStringScanner = new SingleTokenScanner(ScalaSyntaxClasses.MULTI_LINE_STRING, getColorManager, scalaPreferenceStore)
   private val xmlTagScanner = new XmlTagScanner(getColorManager, scalaPreferenceStore)
   private val xmlCommentScanner = new XmlCommentScanner(getColorManager, scalaPreferenceStore)
   private val xmlCDATAScanner = new XmlCDATAScanner(getColorManager, scalaPreferenceStore)
   private val xmlPCDATAScanner = new SingleTokenScanner(ScalaSyntaxClasses.DEFAULT, getColorManager, scalaPreferenceStore)
   private val xmlPIScanner = new XmlPIScanner(getColorManager, scalaPreferenceStore)

   override def getTextHover(sv: ISourceViewer, contentType: String, stateMask: Int) = new ScalaHover(getCodeAssist)

   override def getHyperlinkDetectors(sv: ISourceViewer) = {
      val shd = new ScalaHyperlinkDetector
      if (editor != null)
         shd.setContext(editor)
      Array(shd)
   }

   def getCodeAssist: Option[ICodeAssist] = Option(editor) map { editor =>
      val input = editor.getEditorInput
      val provider = editor.getDocumentProvider

      (provider, input) match {
         case (icudp: ICompilationUnitDocumentProvider, _) => icudp getWorkingCopy input
         case (_, icfei: IClassFileEditorInput) => icfei.getClassFile
         case _ => null
      }
   }

   def getProject: IJavaProject = {
      getCodeAssist map (_.asInstanceOf[IJavaElement].getJavaProject) orNull
   }

   /**
    * Replica of JavaSourceViewerConfiguration#getAutoEditStrategies that returns
    * a ScalaAutoIndentStrategy instead of a JavaAutoIndentStrategy.
    *
    * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getAutoEditStrategies(org.eclipse.jface.text.source.ISourceViewer, java.lang.String)
    */
   override def getAutoEditStrategies(sourceViewer: ISourceViewer, contentType: String): Array[IAutoEditStrategy] = {
      val partitioning = getConfiguredDocumentPartitioning(sourceViewer)
      contentType match {
         case IJavaPartitions.JAVA_DOC | IJavaPartitions.JAVA_MULTI_LINE_COMMENT =>
            Array(new JavaDocAutoIndentStrategy(partitioning))
         case IJavaPartitions.JAVA_STRING =>
            Array(new SmartSemicolonAutoEditStrategy(partitioning), new JavaStringAutoIndentStrategy(partitioning))
         case IJavaPartitions.JAVA_CHARACTER | IDocument.DEFAULT_CONTENT_TYPE =>
            Array(new SmartSemicolonAutoEditStrategy(partitioning), new ScalaAutoIndentStrategy(partitioning, getProject, sourceViewer, new JdtPreferenceProvider(getProject)))
         case _ =>
            Array(new ScalaAutoIndentStrategy(partitioning, getProject, sourceViewer, new JdtPreferenceProvider(getProject)))
      }
   }

  override def getContentFormatter(sourceViewer: ISourceViewer) = {
    val formatter = new MultiPassContentFormatter(getConfiguredDocumentPartitioning(sourceViewer), IDocument.DEFAULT_CONTENT_TYPE)
    formatter.setMasterStrategy(new ScalaFormattingStrategy(editor))
    formatter
  }

   override def handlePropertyChangeEvent(event: PropertyChangeEvent) {
      super.handlePropertyChangeEvent(event)
      scalaCodeScanner.adaptToPreferenceChange(event)
      scaladocScanner.adaptToPreferenceChange(event)
      stringScanner.adaptToPreferenceChange(event)
      multiLineStringScanner.adaptToPreferenceChange(event)
      singleLineCommentScanner.adaptToPreferenceChange(event)
      multiLineCommentScanner.adaptToPreferenceChange(event)
      xmlTagScanner.adaptToPreferenceChange(event)
      xmlCommentScanner.adaptToPreferenceChange(event)
      xmlCDATAScanner.adaptToPreferenceChange(event)
      xmlPCDATAScanner.adaptToPreferenceChange(event)
      xmlPIScanner.adaptToPreferenceChange(event)
   }

   override def affectsTextPresentation(event: PropertyChangeEvent) = true

  /** 
   * Returns Scala-IDE version of InformationPresenter. It differs from normal Java 
   * InformationPresenter in that it uses ScalaHover for computing JavadocHovers. 
   * 
   * @return: the standard information presenter with changed JavadocInformationProvider
   */ 
  override def getInformationPresenter(sourceViewer: ISourceViewer) = {      
    val informationPresenter = super.getInformationPresenter(sourceViewer)
    val scalaHover = new ScalaHover(getCodeAssist)
    scalaHover.setEditor(this.getEditor())       

    //the next commented lines are replaced by the explicit reimplementation of the JavaInformationProvider and JavaTypeHover classes through 
    //ScalaInformationProvider and ScalaJavaTypeHover respectively
    
//    val informationProvider = informationPresenter.getInformationProvider(IDocument.DEFAULT_CONTENT_TYPE).asInstanceOf[JavaInformationProvider]    
//    val implementation = 
//      getDeclaredField(classOf[JavaInformationProvider], "fImplementation").
//      get(informationProvider).asInstanceOf[JavaTypeHover]          
//    getDeclaredField(classOf[JavaTypeHover], "fJavadocHover").set(implementation, scalaHover);
    
    import org.eclipse.jface.text.information.InformationPresenter
    val newInformationProvider = new ScalaInformationProvider(getEditor(), scalaHover)
    informationPresenter.asInstanceOf[InformationPresenter].setInformationProvider(newInformationProvider, IDocument.DEFAULT_CONTENT_TYPE)
    informationPresenter;            
  } 

   
  import org.eclipse.ui.IEditorPart;
  import org.eclipse.jface.text.information.IInformationProvider;
  import org.eclipse.jface.text.information.IInformationProviderExtension;
  import org.eclipse.jface.text.information.IInformationProviderExtension2;
    
  /**
   * A reimplementation of the JavaInformationProvider. The only difference is that the field fImplementation is now initialized with a 
   * ScalaJavaTypeHover object
   */
  class ScalaInformationProvider(editor : IEditorPart, scalaHover : ScalaHover) extends IInformationProvider with IInformationProviderExtension with IInformationProviderExtension2 {  
    import org.eclipse.jface.text.IInformationControlCreator;
    import org.eclipse.jface.text.IRegion;
    import org.eclipse.jface.text.ITextViewer;    
    import org.eclipse.jdt.internal.ui.text.JavaWordFinder;
    
    val fImplementation : ScalaJavaTypeHover = 
	  if (editor != null) {
		val scalaJavaTypeHover = new ScalaJavaTypeHover(scalaHover)
		scalaJavaTypeHover.setEditor(editor);
		scalaJavaTypeHover
      } else
		null

	def getSubject(textViewer : ITextViewer, offset : Int) : IRegion = 
	  if (textViewer != null) JavaWordFinder.findWord(textViewer.getDocument(), offset)
	  else null	

    def getInformation(textViewer : ITextViewer, subject : IRegion) : String = {
	  if (fImplementation != null) {
		val s= fImplementation.getHoverInfo(textViewer, subject);
		if (s != null && s.trim().length() > 0) 
		  return s;					   
	    }
	  return null;
    }

    def getInformation2(textViewer : ITextViewer, subject : IRegion) : Object = 
	  if (fImplementation == null) null
	  else fImplementation.getHoverInfo2(textViewer, subject);	

    def getInformationPresenterControlCreator() : IInformationControlCreator = 
      if (fImplementation == null) null
      else fImplementation.getInformationPresenterControlCreator();    
  } 
  
  import org.eclipse.jface.text.ITextHoverExtension;
  import org.eclipse.jface.text.ITextHoverExtension2;
  import org.eclipse.jdt.ui.text.java.hover.IJavaEditorTextHover;
  
  /**
   * A reimplementation of JavaTypeHover. The only difference is that the field fJavadocHover is set to an object of class ScalaHover 
   */
  class ScalaJavaTypeHover(scalaHover : ScalaHover) extends IJavaEditorTextHover with ITextHoverExtension with ITextHoverExtension2 {
	import org.eclipse.jface.text.IInformationControlCreator;
    import org.eclipse.jface.text.IRegion;
    import org.eclipse.jface.text.ITextViewer;
    import org.eclipse.ui.IEditorPart;
    import org.eclipse.jdt.internal.ui.text.java.hover.ProblemHover;
    
	val fProblemHover : AbstractJavaEditorTextHover = new ProblemHover();
	val fJavadocHover : AbstractJavaEditorTextHover = scalaHover;
	var fCurrentHover : AbstractJavaEditorTextHover = null;

	def setEditor(editor : IEditorPart) {
	  fProblemHover.setEditor(editor);
	  fJavadocHover.setEditor(editor);
	  fCurrentHover= null;
	}

	def getHoverRegion(textViewer : ITextViewer, offset : Int) : IRegion = 
	  fJavadocHover.getHoverRegion(textViewer, offset);
	
	def getHoverInfo(textViewer : ITextViewer, hoverRegion : IRegion) : String = 
	  String.valueOf(getHoverInfo2(textViewer, hoverRegion));
	
	def getHoverInfo2(textViewer : ITextViewer, hoverRegion : IRegion) : Object = {
	  val hoverInfo= fProblemHover.getHoverInfo2(textViewer, hoverRegion);
	  if (hoverInfo != null) {
	    fCurrentHover= fProblemHover;
		hoverInfo;
	  } else {
		fCurrentHover= fJavadocHover;
		fJavadocHover.getHoverInfo2(textViewer, hoverRegion);
	  }
	}

	def getHoverControlCreator() : IInformationControlCreator = 
	  if (fCurrentHover == null) null else fCurrentHover.getHoverControlCreator();
	
	def getInformationPresenterControlCreator() : IInformationControlCreator = 
	  if (fCurrentHover == null) null else fCurrentHover.getInformationPresenterControlCreator();	
  }
}
