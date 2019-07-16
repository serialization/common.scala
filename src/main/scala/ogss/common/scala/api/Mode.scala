/*******************************************************************************
 * Copyright 2019 University of Stuttgart
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License.  You may obtain a copy
 * of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
 * License for the specific language governing permissions and limitations under
 * the License.
 ******************************************************************************/
package ogss.common.scala.api

/**
 * Modes for file handling.
 */
sealed abstract class Mode;
sealed abstract class ReadMode extends Mode;
sealed abstract class WriteMode extends Mode;
object Create extends ReadMode;
object Read extends ReadMode;
object Write extends WriteMode;
/**
 * can not be written at all; read only is permanent and must not be changed with a change mode
 */
object ReadOnly extends WriteMode;
