package com.okapi.core
package annotations

case class ParameterInfo(
  name: String,
  paramType: String,
  annotationType: String,
  annotationValue: String,
  isOptional: Boolean,
)

case class MethodInfo(
  name: String,
  httpMethod: String,
  path: String,
  parameters: List[ParameterInfo],
  returnType: String,
)
