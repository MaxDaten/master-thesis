{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
import StateVar
import Data.Word
import Control.Applicative
import Control.Monad
import Misc (str, getCurrentTime)

-- * GLSL Uniforms
type TextureUnit = Int
type GLId 			 = Int
type UniformName = String
data Program 		 = Program GLId

-- | any GLSL 'uniform' variable
type UniformVar = SettableStateVar
-- | GLSL texture sampler like 'sampler2D'
data UniformSampler dim px = UniformSampler TextureUnit (UniformVar (Maybe (Texture dim px)))

data Tex2D
data PixelRGBA
type UniformSampler2D = UniformSampler Tex2D

-- | Captures the OpenGL object identifier
-- with phantom types for type safety, backed by opengl
-- because format and dim is immutable with 'glTexStorage'
data Texture dim px = Texture GLId
-- | RGBA Color (Byte)
data Color = Color Word8 Word8 Word8 Word8

-- | Material is a combination of a material color and
-- a concrete material 'Texture'
data Material dim px = Material
	{ color 	:: Color
	, texture :: Texture dim px
	}

createMaterial :: Color -> FilePath -> IO (Material dim px)
createMaterial = undefined

-- | GL Program Handle
materialUniform :: Program -> TextureUnit -> UniformName -> IO (UniformVar (Material dim px))
materialUniform prog unit varname = do
	colorVar 		<- colorUniform prog (varname ++ "Color")
	textureVar 	<- textureUniform prog unit (varname ++ "Texture")
	return $ SettableStateVar $ \mat -> do
		colorVar   $= color mat
		textureVar $= Just (texture mat)

colorUniform :: Program -> UniformName -> IO (UniformVar Color)
colorUniform = undefined -- see implementation "Yage.Uniform.Material" ~5 lines of code

textureUniform :: Program -> TextureUnit -> UniformName -> IO (UniformVar (Maybe (Texture dim px)))
textureUniform = undefined -- see implementation "Yage.Uniform.Material" ~5 lines of code

intUniform :: Program -> UniformName -> IO (UniformVar Int)
intUniform = undefined

-- * Shader Compilation
data ShaderType = VertexShader | FragmentShader

compileProgram :: String -> ShaderType -> IO Program
compileProgram = undefined

-- * Application

-- | Example GLSL Fragment Shader Code
fragmentSrc :: String
fragmentSrc = [str|
uniform vec4 			MaterialColor;
uniform int       Time;
uniform sampler2D MaterialTexture;
in 			vec2 			inUV; // from one of the previous stages (e.g. vertex shader)
out 		vec4 			fragColor;
void main () { fragColor = MaterialColor * texture(MaterialTexture, inUV); }
|]

data FragmentShaderInterface = FragmentShaderInterface
	{ uMaterial :: UniformVar (Material Tex2D PixelRGBA) 
	, uTime     :: UniformVar Int
	}

fragmentInterface :: Program -> IO FragmentShaderInterface
fragmentInterface prog = FragmentShaderInterface 
	<$> materialUniform prog 1 "Material"
	<*> intUniform prog "Time"

setFragmentShaderUniforms :: IO ()
setFragmentShaderUniforms = do
	-- init
	FragmentShaderInterface{..} <- fragmentInterface =<< compileProgram fragmentSrc FragmentShader
	mat 				<- createMaterial (Color 255 255 0 255) "path/to/texture.png"
	
	-- some looping
	forever $ do
		currentTime <- getCurrentTime
		uTime 		 $= currentTime
		uMaterial  $= mat
