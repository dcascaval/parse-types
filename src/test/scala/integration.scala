package tsparse

import scala.collection.mutable.ArrayBuffer
import org.scalatest.funsuite._

import fastparse._
import JavaWhitespace._ // Ignore whitespace and //, /* */ comment blocks
import fastparse.Parsed.Success
import fastparse.Parsed.Failure

class TestParse extends AnyFunSuite {
  import Parser._
  import TestHelpers._

  test("members and type parameters") {
    parseWhole("constructor(...args: any[]);", constructor(_))
    parseWhole("(a: T, b: T, ...f: k[]) => Object3d", arrowType(_))
    parseWhole("onLoad?: <ObjectType extends Object3D>(object: ObjectType) => void", parameter(_))
    parseWhole("T extends Object3d", typeArgument(_))
    parseWhole(
      "parse<T extends Object3D>(json: any, onLoad?: (object: Object3D) => void): T;",
      functionMember(_)
    )
    parseWhole("extends Material | Material[]", singleExtensionClause(_))
    parseWhole("= Material | Material[]", singleDefaultClause(_))
    parseWhole("f extends A = B", typeArgument(_))
    parseWhole("TMaterial extends Material | Material[] = Material | Material[]", typeArgument(_))
  }

  test("interfaces") {
    parseWhole("{ [key: string]: InstancedBufferGeometry | BufferGeometry }", objectType(_))
    parseWhole(
      """export interface MorphTarget {
        name: string;
        vertices: Vector3[];
    }""",
      topInterface(_)
    )
    parseWhole(
      """export interface Event {
        type: string;
        target?: any;
        [attachment: string]: any;
    }""",
      fullFile(_)
    )
  }

  test("enums") {
    parseWhole(
      """export enum MOUSE {
        LEFT = 0,
        MIDDLE = 1,
        RIGHT = 2,
        ROTATE = 0,
        DOLLY = 1,
        PAN = 2,
    }""",
      fullFile(_)
    )
  }

  test("integration") {
    parseWhole(
      """export class ObjectLoader extends Loader {
        constructor(manager?: LoadingManager);

        load(
            url: string,
            // tslint:disable-next-line:no-unnecessary-generics
            onLoad?: <ObjectType extends Object3D>(object: ObjectType) => void,
            onProgress?: (event: ProgressEvent) => void,
            onError?: (event: Error | ErrorEvent) => void,
        ): void;
        loadAsync<ObjectType extends Object3D>(
            url: string,
            onProgress?: (event: ProgressEvent) => void,
        ): // tslint:disable-next-line:no-unnecessary-generics
        Promise<ObjectType>;
        // tslint:disable-next-line:no-unnecessary-generics
        parse<T extends Object3D>(json: any, onLoad?: (object: Object3D) => void): T;
        // tslint:disable-next-line:no-unnecessary-generics
        parseAsync<T extends Object3D>(json: any): Promise<T>;
        parseGeometries(json: any): { [key: string]: InstancedBufferGeometry | BufferGeometry }; // Array of BufferGeometry or Geometry or Geometry2.
        parseMaterials(json: any, textures: Texture[]): Material[]; // Array of Classes that inherits from Matrial.
        parseAnimations(json: any): AnimationClip[];
        parseImages(json: any, onLoad: () => void): { [key: string]: HTMLImageElement };
        parseImagesAsync(json: any): Promise<{ [key: string]: HTMLImageElement }>;
        parseTextures(json: any, images: any): Texture[];
        parseObject<T extends Object3D>(
            data: any,
            geometries: any[],
            materials: Material[],
            animations: AnimationClip[],
        ): // tslint:disable-next-line:no-unnecessary-generics
        T;
    }""",
      topClass(_)
    )

    parseWhole(
      """
    import {
        Mesh,
        ShaderMaterial,
        WebGLRenderTarget,
        BufferGeometry,
        WebGLRenderer,
        Scene,
        Camera,
        IUniform,
    } from '../../../src/Three';

    export interface ReflectorShader {
        defines: {
            DISTANCE_ATTENUATION: boolean;
            FRESNEL: boolean;
        };
        uniforms: {
            [key: string]: IUniform;
        };
        vertexShader: string;
        fragmentShader: string;
    }

    export interface ReflectorOptions {
        clipBias?: number | undefined;
        textureWidth?: number | undefined;
        textureHeight?: number | undefined;
        color?: number | undefined;
        useDepthTexture?: boolean | undefined;
        shader?: ReflectorShader | undefined;
    }

    export class Reflector<TGeometry extends BufferGeometry = BufferGeometry> extends Mesh<TGeometry> {
        type: 'ReflectorForSSRPass';
        options: ReflectorOptions;

        static ReflectorShader: ReflectorShader;

        needsUpdate: boolean;
        maxDistance: number;
        opacity: number;

        get distanceAttenuation(): boolean;
        set distanceAttenuation(val: boolean);
        get fresnel(): boolean;
        set fresnel(val: boolean);

        material: ShaderMaterial;

        renderTarget: WebGLRenderTarget;

        constructor(geometry: TGeometry, options: ReflectorOptions);

        doRender: (renderer: WebGLRenderer, scene: Scene, camera: Camera) => void;

        getRenderTarget: () => WebGLRenderTarget;
    }
    """,
      allTop(_)
    )
    parseWhole(
      """/**
     * Event object.
     */
    export interface Event {
        type: string;
        target?: any;
        [attachment: string]: any;
    }

    /**
     * JavaScript events for custom objects
     *
     * @source src/core/EventDispatcher.js
     */
    export class EventDispatcher {
        /**
         * Creates eventDispatcher object. It needs to be call with '.call' to add the functionality to an object.
         */
        constructor();

        /**
         * Adds a listener to an event type.
         * @param type The type of event to listen to.
         * @param listener The function that gets called when the event is fired.
         */
        addEventListener(type: string, listener: (event: Event) => void): void;

        /**
         * Checks if listener is added to an event type.
         * @param type The type of event to listen to.
         * @param listener The function that gets called when the event is fired.
         */
        hasEventListener(type: string, listener: (event: Event) => void): boolean;

        /**
         * Removes a listener from an event type.
         * @param type The type of the listener that gets removed.
         * @param listener The listener function that gets removed.
         */
        removeEventListener(type: string, listener: (event: Event) => void): void;

        /**
         * Fire an event type.
         * @param type The type of event that gets fired.
         */
        dispatchEvent(event: { type: string;[attachment: string]: any }): void;
    }
    """,
      fullFile(_)
    )
  }
}
