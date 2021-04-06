package io.opencaesar.oml2owl

import io.opencaesar.oml.DescriptionBundle
import org.semanticweb.owlapi.model.OWLOntology

case class CloseDescriptionBundleToOwl
(d: DescriptionBundle,
 owlOntology: OWLOntology,
 disjointUnions: Boolean,
 owlApi: OwlApi):

  def run(): Unit =
    ()
