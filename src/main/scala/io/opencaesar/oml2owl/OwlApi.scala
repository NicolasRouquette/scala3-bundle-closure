package io.opencaesar.oml2owl

import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.vocab.OWLFacet

import java.math.BigDecimal
import java.util
import java.util.stream.Collectors
import java.util.{Arrays, Collections, List}

case class OwlApi
( manager: OWLOntologyManager,
  annotationsOnAxioms: Boolean ):

  val factory: OWLDataFactory = manager.getOWLDataFactory

  def addAxiom(o: OWLOntology, a: OWLAxiom): ChangeApplied =
    manager.addAxiom(o, a)

  def getOWLThing: OWLClass =
    factory.getOWLThing

  def getOWLNothing: OWLClass =
    factory.getOWLNothing

  def getOWLClass(iri: IRI): OWLClass =
    factory.getOWLClass(iri)

  def getOWLDataProperty(iri: IRI): OWLDataProperty =
    factory.getOWLDataProperty(iri)

  def getOWLObjectProperty(iri: IRI): OWLObjectProperty =
    factory.getOWLObjectProperty(iri)

  def getOWLNamedIndividual(iri: IRI): OWLNamedIndividual =
    factory.getOWLNamedIndividual(iri)

  def getOWLObjectInverseOf(property: OWLObjectProperty): OWLObjectInverseOf =
    factory.getOWLObjectInverseOf(property)

  def getOWLClassAssertionAxiom(ce: OWLClassExpression, i: OWLIndividual): OWLClassAssertionAxiom =
    factory.getOWLClassAssertionAxiom(ce, i)

  def getOWLObjectComplementOf(e: OWLClassExpression): OWLObjectComplementOf =
    factory.getOWLObjectComplementOf(e)

  def getOWLObjectIntersectionOf(operands: util.stream.Stream[OWLClassExpression]): OWLObjectIntersectionOf =
    factory.getOWLObjectIntersectionOf(operands)

  def getOWLObjectUnionOf(operands: util.stream.Stream[OWLClassExpression]): OWLObjectUnionOf =
    factory.getOWLObjectUnionOf(operands)

  def getOWLDisjointClassesAxiom(operands: util.stream.Stream[OWLClassExpression]): OWLDisjointClassesAxiom =
    factory.getOWLDisjointClassesAxiom(operands)

  def getOWLEquivalentClassesAxiom(operands: util.stream.Stream[OWLClassExpression]): OWLEquivalentClassesAxiom =
    factory.getOWLEquivalentClassesAxiom(operands)

  def getOWLDisjointUnionAxiom(c: OWLClass, operands: util.stream.Stream[OWLClassExpression]): OWLDisjointUnionAxiom =
    factory.getOWLDisjointUnionAxiom(c, operands)

  def getOWLDataMaxCardinality(cardinality: Int, pe: OWLDataPropertyExpression): OWLDataMaxCardinality =
    factory.getOWLDataMaxCardinality(cardinality, pe)

  def getOWLObjectMaxCardinality(cardinality: Int, pe: OWLObjectPropertyExpression): OWLObjectMaxCardinality =
    factory.getOWLObjectMaxCardinality(cardinality, pe)

  def createIri(iri: String): IRI =
    IRI.create(iri)

  def createOntology(iri: String): OWLOntology =
    manager.createOntology(IRI.create(iri))

  def addImportsDeclaration(ontology: OWLOntology, iri: String) =
    val i: OWLImportsDeclaration = factory.getOWLImportsDeclaration(IRI.create(iri))
    manager.applyChanges(new AddImport(ontology, i))
    i

  def addClass(ontology: OWLOntology, iri: String): OWLClass =
    val c: OWLClass = factory.getOWLClass(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(c)
    manager.addAxiom(ontology, axiom)
    c

  def addDatatype(ontology: OWLOntology, iri: String): OWLDatatype =
    val datatype: OWLDatatype = factory.getOWLDatatype(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(datatype)
    manager.addAxiom(ontology, axiom)
    datatype

  def addDatatypeDefinition(ontology: OWLOntology, datatypeIri: String, restrictedIri: String, restrictions: util.Collection[OWLFacetRestriction], annotations: OWLAnnotation*): OWLDatatypeDefinitionAxiom =
    val datatype: OWLDatatype = factory.getOWLDatatype(datatypeIri)
    val restrictedDatatype: OWLDatatype = factory.getOWLDatatype(restrictedIri)
    val restriction: OWLDatatypeRestriction = factory.getOWLDatatypeRestriction(restrictedDatatype, restrictions)
    val axiom: OWLDatatypeDefinitionAxiom = factory.getOWLDatatypeDefinitionAxiom(datatype, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addDatatypeDefinition(ontology: OWLOntology, datatypeIri: String, synonymIri: String, annotations: OWLAnnotation*): OWLDatatypeDefinitionAxiom =
    val datatype: OWLDatatype = factory.getOWLDatatype(datatypeIri)
    val synonymDatatype: OWLDatatype = factory.getOWLDatatype(synonymIri)
    val axiom: OWLDatatypeDefinitionAxiom = factory.getOWLDatatypeDefinitionAxiom(datatype, synonymDatatype, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addDataOneOf(ontology: OWLOntology, subIri: String, literals: OWLLiteral*): OWLDataOneOf =
    val datatype: OWLDatatype = factory.getOWLDatatype(subIri)
    val dataOneOf: OWLDataOneOf = factory.getOWLDataOneOf(literals: _*)
    val axiom: OWLDatatypeDefinitionAxiom = factory.getOWLDatatypeDefinitionAxiom(datatype, dataOneOf)
    manager.addAxiom(ontology, axiom)
    dataOneOf

  def addObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addFunctionalObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLFunctionalObjectPropertyAxiom = factory.getOWLFunctionalObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addInverseFunctionalObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLInverseFunctionalObjectPropertyAxiom = factory.getOWLInverseFunctionalObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addSymmetricObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLSymmetricObjectPropertyAxiom = factory.getOWLSymmetricObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addAsymmetricObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLAsymmetricObjectPropertyAxiom = factory.getOWLAsymmetricObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addReflexiveObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLReflexiveObjectPropertyAxiom = factory.getOWLReflexiveObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addIrreflexiveObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLIrreflexiveObjectPropertyAxiom = factory.getOWLIrreflexiveObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addTransitiveObjectProperty(ontology: OWLOntology, iri: String): OWLObjectProperty =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(iri)
    val axiom: OWLTransitiveObjectPropertyAxiom = factory.getOWLTransitiveObjectPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addDataProperty(ontology: OWLOntology, iri: String): OWLDataProperty =
    val property: OWLDataProperty = factory.getOWLDataProperty(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addFunctionalDataProperty(ontology: OWLOntology, iri: String): OWLDataProperty =
    val property: OWLDataProperty = factory.getOWLDataProperty(iri)
    val axiom: OWLFunctionalDataPropertyAxiom = factory.getOWLFunctionalDataPropertyAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addAnnotationProperty(ontology: OWLOntology, iri: String): OWLAnnotationProperty =
    val property: OWLAnnotationProperty = factory.getOWLAnnotationProperty(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(property)
    manager.addAxiom(ontology, axiom)
    property

  def addNamedIndividual(ontology: OWLOntology, iri: String): OWLNamedIndividual =
    val individual: OWLNamedIndividual = factory.getOWLNamedIndividual(iri)
    val axiom: OWLDeclarationAxiom = factory.getOWLDeclarationAxiom(individual)
    manager.addAxiom(ontology, axiom)
    individual

  def getNamedIndividual(nodeId: String): OWLNamedIndividual = factory.getOWLNamedIndividual(nodeId)

  def getAnonymousIndividual(nodeId: String): OWLAnonymousIndividual = factory.getOWLAnonymousIndividual(nodeId)

  def addRule(ontology: OWLOntology, head: util.List[SWRLAtom], body: util.List[SWRLAtom], annotations: OWLAnnotation*): SWRLRule =
    val axiom: SWRLRule = factory.getSWRLRule(body, head, util.Arrays.asList(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def getClassAtom(classIri: String, variableIri: String): SWRLClassAtom =
    val c: OWLClass = factory.getOWLClass(classIri)
    val variable: SWRLVariable = factory.getSWRLVariable(variableIri)
    factory.getSWRLClassAtom(c, variable)

  def getObjectPropertyAtom(propertyIri: String, variable1Iri: String, variable2Iri: String): SWRLObjectPropertyAtom =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val variable1: SWRLVariable = factory.getSWRLVariable(variable1Iri)
    val variable2: SWRLVariable = factory.getSWRLVariable(variable2Iri)
    factory.getSWRLObjectPropertyAtom(property, variable1, variable2)

  def getSameIndividualAtom(variable1Iri: String, variable2Iri: String): SWRLSameIndividualAtom =
    val variable1: SWRLVariable = factory.getSWRLVariable(variable1Iri)
    val variable2: SWRLVariable = factory.getSWRLVariable(variable2Iri)
    factory.getSWRLSameIndividualAtom(variable1, variable2)

  def getDifferentIndividualsAtom(variable1Iri: String, variable2Iri: String): SWRLDifferentIndividualsAtom =
    val variable1: SWRLVariable = factory.getSWRLVariable(variable1Iri)
    val variable2: SWRLVariable = factory.getSWRLVariable(variable2Iri)
    factory.getSWRLDifferentIndividualsAtom(variable1, variable2)

  def addSubClassOf(ontology: OWLOntology, subIri: String, superIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom =
    val subClass: OWLClass = factory.getOWLClass(subIri)
    val supClass: OWLClass = factory.getOWLClass(superIri)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(subClass, supClass, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addHasKey(ontology: OWLOntology, classIri: String, keyPropertyIris: util.List[String], annotations: OWLAnnotation*): OWLHasKeyAxiom =
    val aClass: OWLClass = factory.getOWLClass(classIri)
    val keyProperties: util.List[OWLDataProperty] = keyPropertyIris.stream.map((iri: String) => factory.getOWLDataProperty(iri)).collect(Collectors.toList)
    val axiom: OWLHasKeyAxiom = factory.getOWLHasKeyAxiom(aClass, keyProperties, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addObjectSomeValuesFrom(ontology: OWLOntology, classIri: String, propertyIri: String, typeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val t: OWLClass = factory.getOWLClass(typeIri)
    val restriction: OWLObjectSomeValuesFrom = factory.getOWLObjectSomeValuesFrom(property, t)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectAllValuesFrom(ontology: OWLOntology, classIri: String, propertyIri: String, typeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val t: OWLClass = factory.getOWLClass(typeIri)
    val restriction: OWLObjectAllValuesFrom = factory.getOWLObjectAllValuesFrom(property, t)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectHasValue(ontology: OWLOntology, classIri: String, propertyIri: String, individual: OWLIndividual, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val restriction: OWLObjectHasValue = factory.getOWLObjectHasValue(property, individual)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectHasValue(ontology: OWLOntology, classIri: String, propertyIri: String, individualIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val individual: OWLNamedIndividual = factory.getOWLNamedIndividual(individualIri)
    val restriction: OWLObjectHasValue = factory.getOWLObjectHasValue(property, individual)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectExactCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    var restriction: OWLObjectExactCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLObjectExactCardinality(cardinality, property, factory.getOWLClass(rangeIri))
    else
      restriction = factory.getOWLObjectExactCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectMinCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    var restriction: OWLObjectMinCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLObjectMinCardinality(cardinality, property, factory.getOWLClass(rangeIri))
    else
      restriction = factory.getOWLObjectMinCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectMaxCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    var restriction: OWLObjectMaxCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLObjectMaxCardinality(cardinality, property, factory.getOWLClass(rangeIri))
    else
      restriction = factory.getOWLObjectMaxCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataExactCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    var restriction: OWLDataExactCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLDataExactCardinality(cardinality, property, factory.getOWLDatatype(rangeIri))
    else
      restriction = factory.getOWLDataExactCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataMinCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    var restriction: OWLDataMinCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLDataMinCardinality(cardinality, property, factory.getOWLDatatype(rangeIri))
    else
      restriction = factory.getOWLDataMinCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataMaxCardinality(ontology: OWLOntology, classIri: String, propertyIri: String, cardinality: Int, rangeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    var restriction: OWLDataMaxCardinality = null
    if (rangeIri != null) then
      restriction = factory.getOWLDataMaxCardinality(cardinality, property, factory.getOWLDatatype(rangeIri))
    else
      restriction = factory.getOWLDataMaxCardinality(cardinality, property)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataSomeValuesFrom(ontology: OWLOntology, classIri: String, propertyIri: String, typeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val t: OWLDatatype = factory.getOWLDatatype(typeIri)
    val restriction: OWLDataSomeValuesFrom = factory.getOWLDataSomeValuesFrom(property, t)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataAllValuesFrom(ontology: OWLOntology, classIri: String, propertyIri: String, typeIri: String, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val t: OWLDatatype = factory.getOWLDatatype(typeIri)
    val restriction: OWLDataAllValuesFrom = factory.getOWLDataAllValuesFrom(property, t)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addDataHasValue(ontology: OWLOntology, classIri: String, propertyIri: String, literal: OWLLiteral, annotations: OWLAnnotation*): OWLSubClassOfAxiom = {
    val c: OWLClass = factory.getOWLClass(classIri)
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val restriction: OWLDataHasValue = factory.getOWLDataHasValue(property, literal)
    val axiom: OWLSubClassOfAxiom = factory.getOWLSubClassOfAxiom(c, restriction, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addSubObjectPropertyOf(ontology: OWLOntology, subPropertyIri: String, superPropertyIri: String, annotations: OWLAnnotation*): OWLSubObjectPropertyOfAxiom = {
    val subProperty: OWLObjectProperty = factory.getOWLObjectProperty(subPropertyIri)
    val supProperty: OWLObjectProperty = factory.getOWLObjectProperty(superPropertyIri)
    val axiom: OWLSubObjectPropertyOfAxiom = factory.getOWLSubObjectPropertyOfAxiom(subProperty, supProperty, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectPropertyDomain(ontology: OWLOntology, propertyIri: String, domainIri: String): OWLObjectPropertyDomainAxiom = {
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val domain: OWLClass = factory.getOWLClass(domainIri)
    val axiom: OWLObjectPropertyDomainAxiom = factory.getOWLObjectPropertyDomainAxiom(property, domain)
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addObjectPropertyRange(ontology: OWLOntology, propertyIri: String, rangeIri: String): OWLObjectPropertyRangeAxiom = {
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val range: OWLClass = factory.getOWLClass(rangeIri)
    val axiom: OWLObjectPropertyRangeAxiom = factory.getOWLObjectPropertyRangeAxiom(property, range)
    manager.addAxiom(ontology, axiom)
    axiom
  }

  def addSubDataPropertyOf(ontology: OWLOntology, subPropertyIri: String, superPropertyIri: String, annotations: OWLAnnotation*): OWLSubDataPropertyOfAxiom =
    val subProperty: OWLDataProperty = factory.getOWLDataProperty(subPropertyIri)
    val supProperty: OWLDataProperty = factory.getOWLDataProperty(superPropertyIri)
    val axiom: OWLSubDataPropertyOfAxiom = factory.getOWLSubDataPropertyOfAxiom(subProperty, supProperty, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addDataPropertyDomain(ontology: OWLOntology, propertyIri: String, domainIri: String): OWLDataPropertyDomainAxiom =
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val domain: OWLClass = factory.getOWLClass(domainIri)
    val axiom: OWLDataPropertyDomainAxiom = factory.getOWLDataPropertyDomainAxiom(property, domain)
    manager.addAxiom(ontology, axiom)
    axiom

  def addDataPropertyRange(ontology: OWLOntology, propertyIri: String, rangeIri: String): OWLDataPropertyRangeAxiom =
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val range: OWLDatatype = factory.getOWLDatatype(rangeIri)
    val axiom: OWLDataPropertyRangeAxiom = factory.getOWLDataPropertyRangeAxiom(property, range)
    manager.addAxiom(ontology, axiom)
    axiom

  def addInverseProperties(ontology: OWLOntology, forwardPropertyIri: String, reversePropertyIri: String): OWLInverseObjectPropertiesAxiom =
    val forwardproperty: OWLObjectProperty = factory.getOWLObjectProperty(forwardPropertyIri)
    val reverseProperty: OWLObjectProperty = factory.getOWLObjectProperty(reversePropertyIri)
    val axiom: OWLInverseObjectPropertiesAxiom = factory.getOWLInverseObjectPropertiesAxiom(forwardproperty, reverseProperty)
    manager.addAxiom(ontology, axiom)
    axiom

  def addClassAssertion(ontology: OWLOntology, individualIri: String, classIri: String, annotations: OWLAnnotation*): OWLClassAssertionAxiom =
    val individual: OWLNamedIndividual = factory.getOWLNamedIndividual(individualIri)
    val c: OWLClass = factory.getOWLClass(classIri)
    val axiom: OWLClassAssertionAxiom = factory.getOWLClassAssertionAxiom(c, individual, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addObjectPropertyAssertion(ontology: OWLOntology, individualIri: String, propertyIri: String, objectIri: String, annotations: OWLAnnotation*): OWLObjectPropertyAssertionAxiom =
    val `object`: OWLNamedIndividual = factory.getOWLNamedIndividual(objectIri)
    val individual: OWLNamedIndividual = factory.getOWLNamedIndividual(individualIri)
    addObjectPropertyAssertion(ontology, individual, propertyIri, `object`, annotations: _*)

  def addObjectPropertyAssertion(ontology: OWLOntology, individual: OWLIndividual, propertyIri: String, `object`: OWLIndividual, annotations: OWLAnnotation*): OWLObjectPropertyAssertionAxiom =
    val property: OWLObjectProperty = factory.getOWLObjectProperty(propertyIri)
    val axiom: OWLObjectPropertyAssertionAxiom = factory.getOWLObjectPropertyAssertionAxiom(property, individual, `object`, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def addDataPropertyAssertion(ontology: OWLOntology, individualIri: String, propertyIri: String, literal: OWLLiteral, annotations: OWLAnnotation*): OWLDataPropertyAssertionAxiom =
    val individual: OWLNamedIndividual = factory.getOWLNamedIndividual(individualIri)
    addDataPropertyAssertion(ontology, individual, propertyIri, literal, annotations: _*)

  def addDataPropertyAssertion(ontology: OWLOntology, individual: OWLIndividual, propertyIri: String, literal: OWLLiteral, annotations: OWLAnnotation*): OWLDataPropertyAssertionAxiom =
    val property: OWLDataProperty = factory.getOWLDataProperty(propertyIri)
    val axiom: OWLDataPropertyAssertionAxiom = factory.getOWLDataPropertyAssertionAxiom(property, individual, literal, checkIfNeeded(annotations: _*))
    manager.addAxiom(ontology, axiom)
    axiom

  def getAnnotation(propertyIri: String, value: OWLAnnotationValue): OWLAnnotation =
    val property: OWLAnnotationProperty = factory.getOWLAnnotationProperty(propertyIri)
    factory.getOWLAnnotation(property, value)

  def addOntologyAnnotation(ontology: OWLOntology, annotation: OWLAnnotation): OWLAnnotation =
    val _addOntologyAnnotation: AddOntologyAnnotation = new AddOntologyAnnotation(ontology, annotation)
    manager.applyChange(_addOntologyAnnotation)
    annotation

  def addAnnotationAssertion(ontology: OWLOntology, subjectIri: String, annotation: OWLAnnotation): OWLAnnotationAssertionAxiom =
    val axiom: OWLAnnotationAssertionAxiom = factory.getOWLAnnotationAssertionAxiom(IRI.create(subjectIri), annotation)
    manager.addAxiom(ontology, axiom)
    axiom

  def getFacetRestriction(facet: OWLFacet, value: OWLLiteral): OWLFacetRestriction =
    factory.getOWLFacetRestriction(facet, value)

  def getLiteral(value: Boolean): OWLLiteral = factory.getOWLLiteral(value)

  def getLiteral(value: Int): OWLLiteral = factory.getOWLLiteral(value)

  def getLiteral(value: Long): OWLLiteral = factory.getOWLLiteral(value.toInt)

  def getLiteral(value: Double): OWLLiteral = factory.getOWLLiteral(value)

  def getLiteral(value: String): OWLLiteral = factory.getOWLLiteral(value)

  def getLiteral(value: BigDecimal): OWLLiteral =
    getLiteralWithDatatype(value.toString, io.opencaesar.oml.util.OmlConstants.XSD_NS + "decimal")

  def getLiteralWithDatatype(value: String, datatypeIri: String): OWLLiteral =
    val datatype: OWLDatatype = factory.getOWLDatatype(datatypeIri)
    factory.getOWLLiteral(value, datatype)

  def getLiteralWithLangTag(value: String, langTag: String): OWLLiteral =
    factory.getOWLLiteral(value, langTag)

  def checkIfNeeded(annotations: OWLAnnotation*): util.List[OWLAnnotation] =
    if (annotationsOnAxioms) then
      util.Arrays.asList(annotations: _*)
    else
      Collections.emptyList