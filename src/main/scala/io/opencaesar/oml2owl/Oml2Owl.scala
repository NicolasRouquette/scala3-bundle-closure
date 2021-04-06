package io.opencaesar.oml2owl

import java.util
import util.Collections
import util.stream.Collectors
import util.stream.StreamSupport
import org.eclipse.emf.ecore.EObject
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.rdf4j.model.vocabulary.RDFS
import org.semanticweb.owlapi.model.OWLAnnotation
import org.semanticweb.owlapi.model.OWLAnonymousIndividual
import org.semanticweb.owlapi.model.OWLFacetRestriction
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.SWRLAtom
import org.semanticweb.owlapi.model.SWRLObjectPropertyAtom
import org.semanticweb.owlapi.vocab.OWLFacet
import io.opencaesar.oml.Annotation
import io.opencaesar.oml.AnnotationProperty
import io.opencaesar.oml.Aspect
import io.opencaesar.oml.Assertion
import io.opencaesar.oml.BooleanLiteral
import io.opencaesar.oml.CardinalityRestrictionKind
import io.opencaesar.oml.Concept
import io.opencaesar.oml.ConceptInstance
import io.opencaesar.oml.ConceptInstanceReference
import io.opencaesar.oml.ConceptTypeAssertion
import io.opencaesar.oml.DecimalLiteral
import io.opencaesar.oml.Description
import io.opencaesar.oml.DescriptionBundle
import io.opencaesar.oml.DifferentFromPredicate
import io.opencaesar.oml.DoubleLiteral
import io.opencaesar.oml.Element
import io.opencaesar.oml.EntityPredicate
import io.opencaesar.oml.EnumeratedScalar
import io.opencaesar.oml.FacetedScalar
import io.opencaesar.oml.Import
import io.opencaesar.oml.IntegerLiteral
import io.opencaesar.oml.KeyAxiom
import io.opencaesar.oml.LinkAssertion
import io.opencaesar.oml.Literal
import io.opencaesar.oml.Member
import io.opencaesar.oml.NamedInstance
import io.opencaesar.oml.Ontology
import io.opencaesar.oml.Predicate
import io.opencaesar.oml.PropertyValueAssertion
import io.opencaesar.oml.QuotedLiteral
import io.opencaesar.oml.RangeRestrictionKind
import io.opencaesar.oml.Reference
import io.opencaesar.oml.RelationCardinalityRestrictionAxiom
import io.opencaesar.oml.RelationEntity
import io.opencaesar.oml.RelationEntityPredicate
import io.opencaesar.oml.RelationInstance
import io.opencaesar.oml.RelationInstanceReference
import io.opencaesar.oml.RelationPredicate
import io.opencaesar.oml.RelationRangeRestrictionAxiom
import io.opencaesar.oml.RelationTargetRestrictionAxiom
import io.opencaesar.oml.RelationTypeAssertion
import io.opencaesar.oml.ReverseRelation
import io.opencaesar.oml.Rule
import io.opencaesar.oml.SameAsPredicate
import io.opencaesar.oml.ScalarProperty
import io.opencaesar.oml.ScalarPropertyCardinalityRestrictionAxiom
import io.opencaesar.oml.ScalarPropertyRangeRestrictionAxiom
import io.opencaesar.oml.ScalarPropertyValueAssertion
import io.opencaesar.oml.ScalarPropertyValueRestrictionAxiom
import io.opencaesar.oml.SpecializationAxiom
import io.opencaesar.oml.Structure
import io.opencaesar.oml.StructureInstance
import io.opencaesar.oml.StructuredProperty
import io.opencaesar.oml.StructuredPropertyCardinalityRestrictionAxiom
import io.opencaesar.oml.StructuredPropertyRangeRestrictionAxiom
import io.opencaesar.oml.StructuredPropertyValueAssertion
import io.opencaesar.oml.StructuredPropertyValueRestrictionAxiom
import io.opencaesar.oml.Term
import io.opencaesar.oml.Vocabulary
import io.opencaesar.oml.VocabularyBundle
import io.opencaesar.oml.util.OmlRead
import io.opencaesar.oml.util.OmlVisitor

import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*

import scala.collection.mutable

object Oml2Owl:

	val BUILT_IN_ONTOLOGIES: List[String] = List(
		"http://www.w3.org/2001/XMLSchema",
		"http://www.w3.org/1999/02/22-rdf-syntax-ns",
		"http://www.w3.org/2000/01/rdf-schema",
		"http://www.w3.org/2002/07/owl")


	def isBuiltInOntology(iri: String): Boolean =
		Oml2Owl.BUILT_IN_ONTOLOGIES.contains(iri)

	private def toFirstUpper(s: String): String =
		if s == null || s.length == 0 then
			s
		else if Character.isUpperCase(s.charAt(0)) then
			s
		else if s.length == 1 then
			s.toUpperCase
		else
			s.substring(0, 1).toUpperCase + s.substring(1)


case class Oml2Owl(inputResource: Resource, owl: OwlApi) extends OmlVisitor[Unit]:

	var ontology: OWLOntology = null

	def run: OWLOntology =
		inputResource.getAllContents.asScala.foreach(doSwitch(_))
		ontology

	override def caseAnnotation(annotation: Annotation): Unit =
		addsAnnotation(OmlRead.getAnnotatedElement(annotation), annotation)
		()

	override def caseVocabulary(vocabulary: Vocabulary): Unit =
		ontology = owl.createOntology(vocabulary.getIri)
		owl.addOntologyAnnotation(ontology, owl.getAnnotation(OmlConstants.ontologyType, owl.createIri(OmlConstants.Vocabulary)))
		()

	override def caseVocabularyBundle(bundle: VocabularyBundle): Unit =
		ontology = owl.createOntology(bundle.getIri)
		owl.addOntologyAnnotation(ontology, owl.getAnnotation(OmlConstants.ontologyType, owl.createIri(OmlConstants.VocabularyBundle)))
		()

	override def caseDescription(description: Description): Unit =
		ontology = owl.createOntology(description.getIri)
		owl.addOntologyAnnotation(ontology, owl.getAnnotation(OmlConstants.ontologyType, owl.createIri(OmlConstants.Description)))
		()

	override def caseDescriptionBundle(bundle: DescriptionBundle): Unit =
		ontology = owl.createOntology(bundle.getIri)
		owl.addOntologyAnnotation(ontology, owl.getAnnotation(OmlConstants.ontologyType, owl.createIri(OmlConstants.DescriptionBundle)))
		()

	override def caseAspect(aspect: Aspect): Unit =
		owl.addClass(ontology, OmlRead.getIri(aspect))
		owl.addSubClassOf(ontology, OmlRead.getIri(aspect), OmlConstants.Aspect)
		()

	override def caseConcept(concept: Concept): Unit =
		owl.addClass(ontology, OmlRead.getIri(concept))
		owl.addSubClassOf(ontology, OmlRead.getIri(concept), OmlConstants.Concept)
		()

	override def caseRelationEntity(entity: RelationEntity): Unit =
		owl.addClass(ontology, OmlRead.getIri(entity))
		owl.addSubClassOf(ontology, OmlRead.getIri(entity), OmlConstants.RelationEntity)
		// source relation
		val sourceRelationIri: String = getSourceIri(entity)
		owl.addObjectProperty(ontology, sourceRelationIri)
		owl.addSubObjectPropertyOf(ontology, sourceRelationIri, OmlConstants.sourceRelation)
		owl.addObjectPropertyDomain(ontology, sourceRelationIri, OmlRead.getIri(entity))
		owl.addObjectPropertyRange(ontology, sourceRelationIri, OmlRead.getIri(entity.getSource))
		owl.addFunctionalObjectProperty(ontology, sourceRelationIri)

		if (entity.isFunctional) then
			owl.addInverseFunctionalObjectProperty(ontology, sourceRelationIri)

		if (entity.isInverseFunctional) then
			owl.addFunctionalObjectProperty(ontology, sourceRelationIri)

		// inverse source relation
		val inverseSourceRelationIri: String = getInverseSourceIri(entity)

		if (inverseSourceRelationIri != null) then
			owl.addObjectProperty(ontology, inverseSourceRelationIri)
			owl.addSubObjectPropertyOf(ontology, inverseSourceRelationIri, OmlConstants.inverseSourceRelation)
			owl.addInverseProperties(ontology, inverseSourceRelationIri, sourceRelationIri)

		// target relation
		val targetRelationIri: String = getTargetIri(entity)
		owl.addObjectProperty(ontology, targetRelationIri)
		owl.addSubObjectPropertyOf(ontology, targetRelationIri, OmlConstants.targetRelation)
		owl.addObjectPropertyDomain(ontology, targetRelationIri, OmlRead.getIri(entity))
		owl.addObjectPropertyRange(ontology, targetRelationIri, OmlRead.getIri(entity.getTarget))
		owl.addFunctionalObjectProperty(ontology, targetRelationIri)
		if (entity.isFunctional) then
			owl.addFunctionalObjectProperty(ontology, targetRelationIri)

		if (entity.isInverseFunctional) then
			owl.addInverseFunctionalObjectProperty(ontology, targetRelationIri)

		// inverse target relation
		val inverseTargetRelationIri: String = getInverseTargetIri(entity)
		if (inverseTargetRelationIri != null) then
			owl.addObjectProperty(ontology, inverseTargetRelationIri)
			owl.addSubObjectPropertyOf(ontology, inverseTargetRelationIri, OmlConstants.inverseTargetRelation)
			owl.addInverseProperties(ontology, inverseTargetRelationIri, targetRelationIri)

		// forward relation
		handleForwardRelation(entity)
		// reverse relation
		handleReverseRelation(entity)
		()

	override def caseStructure(structure: Structure): Unit =
		owl.addClass(ontology, OmlRead.getIri(structure))
		owl.addSubClassOf(ontology, OmlRead.getIri(structure), OmlConstants.Structure)
		()

	override def caseFacetedScalar(scalar: FacetedScalar): Unit =
		owl.addDatatype(ontology, OmlRead.getIri(scalar))
		()

	override def caseEnumeratedScalar(scalar: EnumeratedScalar): Unit =
		owl.addDataOneOf(
			ontology,
			OmlRead.getIri(scalar),
			scalar.getLiterals.asScala.map(getLiteral(_)).toSeq: _*)
		()

	override def caseAnnotationProperty(property: AnnotationProperty): Unit =
		owl.addAnnotationProperty(ontology, OmlRead.getIri(property))
		()

	override def caseScalarProperty(property: ScalarProperty): Unit =
		val propertyIri: String = OmlRead.getIri(property)
		owl.addDataProperty(ontology, propertyIri)
		owl.addSubDataPropertyOf(ontology, propertyIri, OmlConstants.scalarProperty)
		owl.addDataPropertyDomain(ontology, propertyIri, OmlRead.getIri(property.getDomain))
		owl.addDataPropertyRange(ontology, propertyIri, OmlRead.getIri(property.getRange))
		if (property.isFunctional) then
			owl.addFunctionalDataProperty(ontology, propertyIri)
		()

	override def caseStructuredProperty(property: StructuredProperty): Unit =
		val propertyIri: String = OmlRead.getIri(property)
		owl.addObjectProperty(ontology, propertyIri)
		owl.addSubObjectPropertyOf(ontology, propertyIri, OmlConstants.structuredProperty)
		owl.addObjectPropertyDomain(ontology, propertyIri, OmlRead.getIri(property.getDomain))
		owl.addObjectPropertyRange(ontology, propertyIri, OmlRead.getIri(property.getRange))
		if (property.isFunctional) then
			owl.addFunctionalObjectProperty(ontology, propertyIri)
		()

	protected def handleForwardRelation(entity: RelationEntity): Unit =
		val forwardName: String = getForwardName(entity)
		val forwardIri: String = getForwardIri(entity)
		owl.addObjectProperty(ontology, forwardIri)
		owl.addSubObjectPropertyOf(ontology, forwardIri, OmlConstants.forwardRelation)
		owl.addObjectPropertyDomain(ontology, forwardIri, OmlRead.getIri(entity.getSource))
		owl.addObjectPropertyRange(ontology, forwardIri, OmlRead.getIri(entity.getTarget))
		if (entity.isFunctional) then
			owl.addFunctionalObjectProperty(ontology, forwardIri)

		if (entity.isInverseFunctional) then
			owl.addInverseFunctionalObjectProperty(ontology, forwardIri)

		if (entity.isSymmetric) then
			owl.addSymmetricObjectProperty(ontology, forwardIri)

		if (entity.isAsymmetric) then
			owl.addAsymmetricObjectProperty(ontology, forwardIri)

		if (entity.isReflexive) then
			owl.addReflexiveObjectProperty(ontology, forwardIri)

		if (entity.isIrreflexive) then
			owl.addIrreflexiveObjectProperty(ontology, forwardIri)

		if (entity.isTransitive) then
			owl.addTransitiveObjectProperty(ontology, forwardIri)

		// derivation rule for forward relation
		val antedecents = new util.ArrayList[SWRLAtom]
		antedecents.add(owl.getObjectPropertyAtom(getSourceIri(entity), getSwrlIri("r"), getSwrlIri("s")))
		antedecents.add(owl.getObjectPropertyAtom(getTargetIri(entity), getSwrlIri("r"), getSwrlIri("t")))
		val consequent: SWRLObjectPropertyAtom = owl.getObjectPropertyAtom(forwardIri, getSwrlIri("s"), getSwrlIri("t"))
		val annotation: OWLAnnotation = owl.getAnnotation(RDFS.LABEL.toString, owl.getLiteral(forwardName + " derivation"))
		owl.addRule(ontology, Collections.singletonList(consequent), antedecents, annotation)

	protected def handleReverseRelation(entity: RelationEntity): Unit =
		if (entity.getReverseRelation != null) then
			val reverse: ReverseRelation = entity.getReverseRelation
			val reverseIri: String = OmlRead.getIri(reverse)
			owl.addObjectProperty(ontology, reverseIri)
			owl.addSubObjectPropertyOf(ontology, reverseIri, OmlConstants.reverseRelation)
			owl.addInverseProperties(ontology, reverseIri, OmlRead.getIri(reverse.getInverse))

	override def caseRule(rule: Rule): Unit =
		val annotations: Seq[OWLAnnotation] = rule.getOwnedAnnotations.asScala.map(createAnnotation(_)).toSeq
		val ruleAnnotations = if !annotations.exists(_.getProperty.getIRI.toString == RDFS.LABEL.toString) then
			owl.getAnnotation(RDFS.LABEL.toString, owl.getLiteral(rule.getName)) +: annotations
		else
			annotations
		owl.addRule(
			ontology,
			rule.getConsequent.stream.flatMap((p: Predicate) => getAtom(p).stream).collect(Collectors.toList),
			rule.getAntecedent.stream.flatMap((p: Predicate) => getAtom(p).stream).collect(Collectors.toList),
			annotations: _*)
		()

	override def caseConceptInstance(instance: ConceptInstance): Unit =
		val instanceIri: String = OmlRead.getIri(instance)
		val individual: OWLIndividual = owl.addNamedIndividual(ontology, instanceIri)
		instance.getOwnedPropertyValues.forEach((it: PropertyValueAssertion) => appliesTo(it, individual))
		instance.getOwnedLinks.forEach((it: LinkAssertion) => appliesTo(it, individual))
		()

	override def caseConceptInstanceReference(reference: ConceptInstanceReference): Unit =
		val instanceIri: String = OmlRead.getIri(OmlRead.resolve(reference))
		val individual: OWLNamedIndividual = owl.getNamedIndividual(instanceIri)
		reference.getOwnedPropertyValues.forEach((it: PropertyValueAssertion) => appliesTo(it, individual))
		reference.getOwnedLinks.forEach((it: LinkAssertion) => appliesTo(it, individual))
		()

	override def caseRelationInstance(instance: RelationInstance): Unit =
		val instanceIri: String = OmlRead.getIri(instance)
		val individual: OWLNamedIndividual = owl.addNamedIndividual(ontology, instanceIri)
		instance.getOwnedPropertyValues.forEach((it: PropertyValueAssertion) => appliesTo(it, individual))
		instance.getOwnedLinks.forEach((it: LinkAssertion) => appliesTo(it, individual))
		()

	override def caseRelationInstanceReference(reference: RelationInstanceReference): Unit =
		val instanceIri: String = OmlRead.getIri(OmlRead.resolve(reference))
		val individual: OWLNamedIndividual = owl.getNamedIndividual(instanceIri)
		reference.getOwnedPropertyValues.forEach((it: PropertyValueAssertion) => appliesTo(it, individual))
		reference.getOwnedLinks.forEach((it: LinkAssertion) => appliesTo(it, individual))
		()

	override def caseImport(i: Import): Unit =
		val importedOntology: Ontology = OmlRead.getImportedOntology(i)
		if (importedOntology != null) then
			val iri: String = importedOntology.getIri
			if (Oml2Owl.isBuiltInOntology(iri)) then
				val indirectImports: util.List[Import] = OmlRead.closure(i, (it: Import) => OmlRead.getImportsWithSource(OmlRead.getImportedOntology(it)))
				indirectImports.forEach((i2: Import) => {
					val iri2: String = OmlRead.getImportedOntology(i2).getIri
					if (!(Oml2Owl.isBuiltInOntology(iri2))) then
							owl.addImportsDeclaration(ontology, iri2)
				})
			else
				owl.addImportsDeclaration(ontology, iri)
		else
			throw new RuntimeException("Could not resolve IRI '" + i.getUri + "'")
		()

	override def caseSpecializationAxiom(axiom: SpecializationAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		specializes(
			OmlRead.getSpecificTerm(axiom),
			axiom.getSpecializedTerm,
			axiom.getOwningReference,
			toArray(annotations): _*)
		()

	override def caseScalarPropertyRangeRestrictionAxiom(axiom: ScalarPropertyRangeRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq RangeRestrictionKind.ALL) then
			owl.addDataAllValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)
		else
			owl.addDataSomeValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)

	override def caseScalarPropertyValueRestrictionAxiom(axiom: ScalarPropertyValueRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addDataHasValue(
			ontology,
			OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
			OmlRead.getIri(axiom.getProperty),
			getLiteral(axiom.getValue),
			toArray(annotations): _*)
		()

	override def caseScalarPropertyCardinalityRestrictionAxiom(axiom: ScalarPropertyCardinalityRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq CardinalityRestrictionKind.MIN) then
			owl.addDataMinCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		else if (axiom.getKind eq CardinalityRestrictionKind.MAX) then
				owl.addDataMaxCardinality(
					ontology,
					OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
					OmlRead.getIri(axiom.getProperty),
					axiom.getCardinality.toInt,
					(if ((axiom.getRange != null)) then
						OmlRead.getIri(axiom.getRange)
					else
						null),
					toArray(annotations): _*)
		else
			owl.addDataExactCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		()

	override def caseStructuredPropertyRangeRestrictionAxiom(axiom: StructuredPropertyRangeRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq RangeRestrictionKind.ALL) then
			owl.addObjectAllValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)
		else
			owl.addObjectSomeValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)
		()

	override def caseStructuredPropertyValueRestrictionAxiom(axiom: StructuredPropertyValueRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addObjectHasValue(
			ontology,
			OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
			OmlRead.getIri(axiom.getProperty),
			createIndividual(axiom.getValue),
			toArray(annotations): _*)
		()

	override def caseStructuredPropertyCardinalityRestrictionAxiom(axiom: StructuredPropertyCardinalityRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq CardinalityRestrictionKind.MIN) then
			owl.addObjectMinCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		else if (axiom.getKind eq CardinalityRestrictionKind.MAX) then
				owl.addObjectMaxCardinality(
					ontology,
					OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
					OmlRead.getIri(axiom.getProperty),
					axiom.getCardinality.toInt,
					(if ((axiom.getRange != null)) then
						OmlRead.getIri(axiom.getRange)
					else
						null),
					toArray(annotations): _*)
		else
			owl.addObjectExactCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getProperty),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		()

	override def caseRelationRangeRestrictionAxiom(axiom: RelationRangeRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq RangeRestrictionKind.ALL) then
			owl.addObjectAllValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getRelation),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)
		else
			owl.addObjectSomeValuesFrom(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getRelation),
				OmlRead.getIri(axiom.getRange),
				toArray(annotations): _*)
		()

	override def caseRelationTargetRestrictionAxiom(axiom: RelationTargetRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addObjectHasValue(
			ontology,
			OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
			OmlRead.getIri(axiom.getRelation),
			OmlRead.getIri(axiom.getTarget),
			toArray(annotations): _*)
		()

	override def caseRelationCardinalityRestrictionAxiom(axiom: RelationCardinalityRestrictionAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		if (axiom.getKind eq CardinalityRestrictionKind.MIN) then
			owl.addObjectMinCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getRelation),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		else if (axiom.getKind eq CardinalityRestrictionKind.MAX) then
				owl.addObjectMaxCardinality(
					ontology,
					OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
					OmlRead.getIri(axiom.getRelation),
					axiom.getCardinality.toInt,
					(if ((axiom.getRange != null)) then
						OmlRead.getIri(axiom.getRange)
					else
						null),
					toArray(annotations): _*)
		else
			owl.addObjectExactCardinality(
				ontology,
				OmlRead.getIri(OmlRead.getRestrictingClassifier(axiom)),
				OmlRead.getIri(axiom.getRelation),
				axiom.getCardinality.toInt,
				(if ((axiom.getRange != null)) then
					OmlRead.getIri(axiom.getRange)
				else
					null),
				toArray(annotations): _*)
		()

	override def caseKeyAxiom(axiom: KeyAxiom): Unit =
		val annotations: util.List[OWLAnnotation] = axiom.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addHasKey(
			ontology,
			OmlRead.getIri(OmlRead.getKeyedEntity(axiom)),
			axiom.getProperties.stream.map((i: ScalarProperty) => OmlRead.getIri(i)).collect(Collectors.toList),
			toArray(annotations): _*)
		()

	override def caseConceptTypeAssertion(assertion: ConceptTypeAssertion): Unit =
		val annotations: util.List[OWLAnnotation] = assertion.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addClassAssertion(
			ontology,
			OmlRead.getIri(OmlRead.getConceptInstance(assertion)),
			OmlRead.getIri(assertion.getType),
			toArray(annotations): _*)
		()

	override def caseRelationTypeAssertion(assertion: RelationTypeAssertion): Unit =
		val annotations: util.List[OWLAnnotation] = assertion.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		val instance: RelationInstance = OmlRead.getRelationInstance(assertion)
		val instanceIri: String = OmlRead.getIri(instance)
		owl.addClassAssertion(
			ontology,
			instanceIri,
			OmlRead.getIri(assertion.getType),
			toArray(annotations): _*)
		instance.getSources.forEach((s: NamedInstance) => owl.addObjectPropertyAssertion(ontology, instanceIri, getSourceIri(assertion.getType), OmlRead.getIri(s)))
		instance.getTargets.forEach((t: NamedInstance) => owl.addObjectPropertyAssertion(ontology, instanceIri, getTargetIri(assertion.getType), OmlRead.getIri(t)))
		()

	protected def createAnnotation(annotation: Annotation): OWLAnnotation =
		var literal: OWLLiteral = null
		if (annotation.getValue != null) then
			literal = getLiteral(annotation.getValue)
		else
			literal = owl.getLiteral("true")
		owl.getAnnotation(OmlRead.getIri(annotation.getProperty), literal)

	protected def addsAnnotation(element: Element, annotation: Annotation): Unit =
		if (element.isInstanceOf[Ontology]) then
			owl.addOntologyAnnotation(ontology, createAnnotation(annotation))
		else if (element.isInstanceOf[Member]) then
			owl.addAnnotationAssertion(ontology, OmlRead.getIri(element.asInstanceOf[Member]), createAnnotation(annotation))
		else if (element.isInstanceOf[Reference]) then
			addsAnnotation(OmlRead.resolve(element.asInstanceOf[Reference]), annotation)

	protected def specializes(specific: Term, general: Term, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		(specific, general) match
			case (s: Aspect, g: Aspect) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: Concept, g: Aspect) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: Concept, g: Concept) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: RelationEntity, g: Aspect) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: RelationEntity, g: RelationEntity) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: EnumeratedScalar, g: EnumeratedScalar) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: FacetedScalar, g: FacetedScalar) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: ScalarProperty, g: ScalarProperty) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: Structure, g: Structure) =>
				specializes(s, g, owningReference, annotations: _*)
			case (s: StructuredProperty, g: StructuredProperty) =>
				specializes(s, g, owningReference, annotations: _*)
			case _ =>
				()

	protected def specializes(specific: Concept, general: Concept, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: Concept, general: Aspect, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: Aspect, general: Aspect, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: RelationEntity, general: RelationEntity, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)
		owl.addSubObjectPropertyOf(ontology, getSourceIri(specific), getSourceIri(general), annotations: _*)
		owl.addSubObjectPropertyOf(ontology, getTargetIri(specific), getTargetIri(general), annotations: _*)
		owl.addSubObjectPropertyOf(ontology, getForwardIri(specific), getForwardIri(general), annotations: _*)

	protected def specializes(specific: RelationEntity, general: Aspect, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: Structure, general: Structure, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubClassOf(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: EnumeratedScalar, general: EnumeratedScalar, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addDatatypeDefinition(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)

	protected def specializes(specific: FacetedScalar, general: FacetedScalar, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		if ((owningReference != null)) then
			owl.addDatatypeDefinition(ontology, OmlRead.getIri(specific), OmlRead.getIri(general), annotations: _*)
		else
			val restrictions: util.ArrayList[OWLFacetRestriction] = new util.ArrayList[OWLFacetRestriction]
			if (specific.getLength != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.LENGTH, owl.getLiteral((specific.getLength).longValue)))
			if (specific.getMaxLength != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MAX_LENGTH, owl.getLiteral((specific.getLength).longValue)))
			if (specific.getMinLength != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MIN_LENGTH, owl.getLiteral((specific.getLength).longValue)))
			if (specific.getPattern != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.PATTERN, owl.getLiteral(specific.getPattern)))
			if (specific.getLanguage != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.LANG_RANGE, owl.getLiteral(specific.getLanguage)))
			if (specific.getMinInclusive != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MIN_INCLUSIVE, getLiteral(specific.getMinInclusive)))
			if (specific.getMaxInclusive != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MAX_INCLUSIVE, getLiteral(specific.getMaxInclusive)))
			if (specific.getMinExclusive != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MIN_EXCLUSIVE, getLiteral(specific.getMinExclusive)))
			if (specific.getMaxExclusive != null) then
				restrictions.add(owl.getFacetRestriction(OWLFacet.MAX_EXCLUSIVE, getLiteral(specific.getMaxExclusive)))
			if (!(restrictions.isEmpty)) then
				owl.addDatatypeDefinition(
					ontology,
					OmlRead.getIri(specific),
					OmlRead.getIri(general),
					restrictions,
					annotations: _*)
			else
				owl.addDatatypeDefinition(
					ontology,
					OmlRead.getIri(specific),
					OmlRead.getIri(general),
					annotations: _*)


	protected def specializes(specific: ScalarProperty, general: ScalarProperty, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubDataPropertyOf(
			ontology,
			OmlRead.getIri(specific),
			OmlRead.getIri(general),
			annotations: _*)

	protected def specializes(specific: StructuredProperty, general: StructuredProperty, owningReference: Reference, annotations: OWLAnnotation*): Unit =
		owl.addSubObjectPropertyOf(
			ontology,
			OmlRead.getIri(specific),
			OmlRead.getIri(general),
			annotations: _*)

	protected def appliesTo(assertion: Assertion, i: OWLIndividual): Unit =
		assertion match
			case a: ScalarPropertyValueAssertion if null != i =>
				appliesTo(a, i)
			case a: StructuredPropertyValueAssertion if null != i =>
				appliesTo(a, i)
			case a: LinkAssertion =>
				i match
						case n: OWLNamedIndividual =>
							appliesTo(a, n)
						case _ =>
							()
			case _ =>
				()

	protected def appliesTo(assertion: ScalarPropertyValueAssertion, individual: OWLIndividual): Unit =
		val annotations: util.List[OWLAnnotation] = assertion.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addDataPropertyAssertion(
			ontology,
			individual,
			OmlRead.getIri(assertion.getProperty),
			getLiteral(assertion.getValue),
			toArray(annotations): _*)

	protected def appliesTo(assertion: StructuredPropertyValueAssertion, individual: OWLIndividual): Unit =
		val annotations: util.List[OWLAnnotation] = assertion.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addObjectPropertyAssertion(
			ontology,
			individual,
			OmlRead.getIri(assertion.getProperty),
			createIndividual(assertion.getValue),
			toArray(annotations): _*)

	protected def appliesTo(assertion: LinkAssertion, individual: OWLNamedIndividual): Unit =
		val annotations: util.List[OWLAnnotation] = assertion.getOwnedAnnotations.stream.map((it: Annotation) => createAnnotation(it)).collect(Collectors.toList)
		owl.addObjectPropertyAssertion(
			ontology,
			individual.getIRI.getIRIString,
			OmlRead.getIri(assertion.getRelation),
			OmlRead.getIri(assertion.getTarget),
			toArray(annotations): _*)

	protected def getAtom(predicate: Predicate): util.List[SWRLAtom] =
		if (predicate.isInstanceOf[DifferentFromPredicate]) then
			getAtom(predicate.asInstanceOf[DifferentFromPredicate])
		else if (predicate.isInstanceOf[EntityPredicate]) then
			getAtom(predicate.asInstanceOf[EntityPredicate])
		else if (predicate.isInstanceOf[RelationEntityPredicate]) then
			getAtom(predicate.asInstanceOf[RelationEntityPredicate])
		else if (predicate.isInstanceOf[RelationPredicate]) then
			getAtom(predicate.asInstanceOf[RelationPredicate])
		else if (predicate.isInstanceOf[SameAsPredicate]) then
			getAtom(predicate.asInstanceOf[SameAsPredicate])
		else
			Collections.emptyList

	protected def getAtom(predicate: EntityPredicate): util.List[SWRLAtom] =
		val atoms: util.List[SWRLAtom] = new util.ArrayList[SWRLAtom]
		atoms.add(owl.getClassAtom(OmlRead.getIri(predicate.getEntity), getSwrlIri(predicate.getVariable)))
		atoms

	protected def getAtom(predicate: RelationEntityPredicate): util.List[SWRLAtom] =
		val atoms: util.List[SWRLAtom] = new util.ArrayList[SWRLAtom]
		atoms.add(owl.getObjectPropertyAtom(getSourceIri(predicate.getEntity), getSwrlIri(predicate.getEntityVariable), getSwrlIri(predicate.getVariable1)))
		atoms.add(owl.getObjectPropertyAtom(getTargetIri(predicate.getEntity), getSwrlIri(predicate.getEntityVariable), getSwrlIri(predicate.getVariable2)))
		atoms

	protected def getAtom(predicate: RelationPredicate): util.List[SWRLAtom] =
		val atoms: util.List[SWRLAtom] = new util.ArrayList[SWRLAtom]
		atoms.add(owl.getObjectPropertyAtom(OmlRead.getIri(predicate.getRelation), getSwrlIri(predicate.getVariable1), getSwrlIri(predicate.getVariable2)))
		atoms

	protected def getAtom(predicate: SameAsPredicate): util.List[SWRLAtom] =
		val atoms: util.List[SWRLAtom] = new util.ArrayList[SWRLAtom]
		atoms.add(owl.getSameIndividualAtom(getSwrlIri(predicate.getVariable1), getSwrlIri(predicate.getVariable2)))
		atoms

	protected def getAtom(predicate: DifferentFromPredicate): util.List[SWRLAtom] =
		val atoms: util.List[SWRLAtom] = new util.ArrayList[SWRLAtom]
		atoms.add(owl.getDifferentIndividualsAtom(getSwrlIri(predicate.getVariable1), getSwrlIri(predicate.getVariable2)))
		atoms

	protected def getLiteral(literal: Literal): OWLLiteral =
		literal match
			case l: BooleanLiteral =>
				getLiteral(l)
			case l: DecimalLiteral =>
				getLiteral(l)
			case l: DoubleLiteral =>
				getLiteral(l)
			case l: IntegerLiteral =>
				getLiteral(l)
			case l: QuotedLiteral =>
				getLiteral(l)

	protected def getLiteral(literal: QuotedLiteral): OWLLiteral =
		if (literal.getType != null) then
			owl.getLiteralWithDatatype(literal.getValue.toString, OmlRead.getIri(literal.getType))
		else if (literal.getLangTag != null) then
			owl.getLiteralWithLangTag(literal.getValue.toString, literal.getLangTag)
		else
			owl.getLiteral(literal.getValue)

	protected def getLiteral(literal: BooleanLiteral): OWLLiteral =
		owl.getLiteral(literal.isValue)

	protected def getLiteral(literal: IntegerLiteral): OWLLiteral =
		owl.getLiteral(literal.getValue)

	protected def getLiteral(literal: DecimalLiteral): OWLLiteral =
		owl.getLiteral(literal.getValue)

	protected def getLiteral(literal: DoubleLiteral): OWLLiteral =
		owl.getLiteral(literal.getValue)

	protected def createIndividual(instance: StructureInstance): OWLAnonymousIndividual =
		val individual: OWLAnonymousIndividual = owl.getAnonymousIndividual(OmlRead.getId(instance))
		instance.getOwnedPropertyValues.forEach((it: PropertyValueAssertion) => appliesTo(it, individual))
		individual

	protected def getForwardName(entity: RelationEntity): String =
		if (entity.getForwardRelation != null) then
			entity.getForwardRelation.getName
		else
			val name: String = Oml2Owl.toFirstUpper(entity.getName)
			"has" + name + "Forward"

	protected def getForwardIri(entity: RelationEntity): String =
		val namespace: String = OmlRead.getNamespace(OmlRead.getOntology(entity))
		val name: String = getForwardName(entity)
		namespace + name

	protected def getSourceIri(entity: RelationEntity): String =
		if (entity.getSourceRelation != null) then
			OmlRead.getIri(entity.getSourceRelation)
		else
			val namespace: String = OmlRead.getNamespace(OmlRead.getOntology(entity))
			val name: String = Oml2Owl.toFirstUpper(entity.getName)
			namespace + "has" + name + "Source"

	protected def getTargetIri(entity: RelationEntity): String =
		if (entity.getTargetRelation != null) then
			OmlRead.getIri(entity.getTargetRelation)
		else
			val namespace: String = OmlRead.getNamespace(OmlRead.getOntology(entity))
			val name: String = Oml2Owl.toFirstUpper(entity.getName)
			namespace + "has" + name + "Target"

	protected def getInverseSourceIri(entity: RelationEntity): String =
		if (entity.getInverseSourceRelation != null) then
			OmlRead.getIri(entity.getInverseSourceRelation)
		else
			""

	protected def getInverseTargetIri(entity: RelationEntity): String =
		if (entity.getInverseTargetRelation != null) then
			OmlRead.getIri(entity.getInverseTargetRelation)
		else
			""

	protected def getSwrlIri(variableName: String): String =
		"urn:swrl#" + variableName

	private def toArray(annotations: util.List[OWLAnnotation]): Array[OWLAnnotation] =
		annotations.toArray(new Array[OWLAnnotation](0))

