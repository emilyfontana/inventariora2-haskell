-- Emily Pontes Fontana 4u noite

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time
import Data.List (sortBy)  --  ordenar a lista de itens, antes os ids estavam bagunçados
import Control.Exception (catch, IOException)
import Control.Monad -- para when
import System.IO
import Text.Read (readMaybe)



-- item do inventario com as descricoes e tipos corretos
data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq)

-- Inventario é um Map de String para Item
type Inventario = Map String Item

-- Tipo de ação indicada na tarefa do prof funcoes crud
data AcaoLog = Add | Remove | Update | QueryFail
  deriving (Show, Read, Eq)

-- status da operacao conforme pede (falha/sucesso)
data StatusLog = Sucesso | Falha String
  deriving (Show, Read, Eq)

-- entrada de log
data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Eq)

-- logica das operacoes

type ResultadoOperacao = (Inventario, LogEntry)

-- Add
addItem :: UTCTime -> String -> String -> Int -> String -> Inventario  
        -> Either String ResultadoOperacao
addItem time itemId nomeItem qtd cat inv -- possiveis erro que nao pode aceitar
  | Map.member itemId inv = Left "Erro: Item ja existe no inventario"
  | qtd <= 0 = Left "Erro: Quantidade deve ser positiva"
  | null itemId = Left "Erro: ID nao pode ser vazio"
  | null nomeItem = Left "Erro: Nome nao pode ser vazio"
  | otherwise =  -- se tudo correto
      let novoItem = Item itemId nomeItem qtd cat
          novoInv = Map.insert itemId novoItem inv
          logEntry = LogEntry time Add 
                     ("Add: " ++ itemId ++ " - " ++ nomeItem ++ " qtd:" ++ show qtd)
                     Sucesso --adiciona certinho
      in Right (novoInv, logEntry)

-- Remover
removeItem :: UTCTime -> String -> Int -> Inventario 
           -> Either String ResultadoOperacao
removeItem time itemId qtd inv
  | not (Map.member itemId inv) = Left "Erro: Item nao encontrado"
  | qtd <= 0 = Left "Erro: Quantidade deve ser positiva"
  | otherwise = -- se estiver tudo certo
      case Map.lookup itemId inv of
        Nothing -> Left "Erro: Item nao encontrado" 
        Just item ->
          if quantidade item < qtd --valor de qtd indicado é maior do q oq contem
            then Left ("Erro: Estoque insuficiente. Disponivel: " ++ show (quantidade item))
            else
              let novaQtd = quantidade item - qtd --nova qtd
                  itemAtualizado = item { quantidade = novaQtd }
                  novoInv = if novaQtd == 0
                           then Map.delete itemId inv
                           else Map.insert itemId itemAtualizado inv
                  logEntry = LogEntry time Remove
                            ("Remove: " ++ itemId ++ " qtd:" ++ show qtd)
                            Sucesso
              in Right (novoInv, logEntry)

-- Atualiza 
updateItem :: UTCTime -> String -> Int -> Inventario 
           -> Either String ResultadoOperacao
updateItem time itemId novaQtd inv
  | not (Map.member itemId inv) = Left "Erro: Item nao encontrado"
  | novaQtd < 0 = Left "Erro: Quantidade nao pode ser negativa"
  | otherwise =
      case Map.lookup itemId inv of
        Nothing -> Left "Erro: Item nao encontrado" --antes mensagem só aparecia depois de informar o id, agora confere corretamente
        Just item ->
          let itemAtualizado = item { quantidade = novaQtd } --atualiza qtd
              novoInv = Map.insert itemId itemAtualizado inv
              logEntry = LogEntry time Update
                        ("Update: " ++ itemId ++ " nova qtd:" ++ show novaQtd)
                        Sucesso
          in Right (novoInv, logEntry)
--files 

inventarioFile :: String
inventarioFile = "Inventario.dat"

auditoriaFile :: String
auditoriaFile = "Auditoria.log"

-- inventario
carregarInventario :: IO Inventario
carregarInventario = do
  catch (do
    conteudo <- readFile inventarioFile
    case readMaybe conteudo of
      Just inv -> return inv
      Nothing -> return Map.empty
    ) (\(_ :: IOException) -> return Map.empty)

-- Salva
salvarInventario :: Inventario -> IO ()
salvarInventario inv = writeFile inventarioFile (show inv)

-- Adiciona log
adicionarLog :: LogEntry -> IO ()
adicionarLog entry = appendFile auditoriaFile (show entry ++ "\n")

-- Carrega log
carregarLogs :: IO [LogEntry]
carregarLogs = do
  catch (do
    conteudo <- readFile auditoriaFile
    let linhas = lines conteudo
    return [entry | linha <- linhas, Just entry <- [readMaybe linha]]
    ) (\(_ :: IOException) -> return [])

-- Criar log de falha
criarLogFalha :: UTCTime -> AcaoLog -> String -> LogEntry
criarLogFalha time acaoTipo mensagem = 
  LogEntry time acaoTipo mensagem (Falha mensagem)

-- Menu principal para interação 
mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn "\n MENU DE AÇÕES "
  putStrLn "1 - Adicionar item"
  putStrLn "2 - Remover item"
  putStrLn "3 - Atualizar item"
  putStrLn "4 - Listar inventario"
  putStrLn "5 - Relatorio"
  putStrLn "0 - Sair"
  putStr "Escolha uma opcao: "
  hFlush stdout

-- Processar opção
processarOpcao :: String -> Inventario -> IO Inventario
processarOpcao opcao inv = do
  time <- getCurrentTime  -- Pega o horário atual uma vez só - ele sai em auditoria log
  
  case opcao of
    "1" -> do
      -- Opção 1: Adicionar item
      putStr "ID: "
      hFlush stdout
      itemId <- getLine
      
      putStr "Nome: "
      hFlush stdout
      nomeItem <- getLine
      
      putStr "Quantidade: "
      hFlush stdout
      qtdStr <- getLine
      
      putStr "Categoria: "
      hFlush stdout
      cat <- getLine
      
      case readMaybe qtdStr of
        Just qtd ->
          case addItem time itemId nomeItem qtd cat inv of
            Right (novoInv, logEntry) -> do
              salvarInventario novoInv
              adicionarLog logEntry
              putStrLn "Item adicionado com sucesso!"
              return novoInv
            Left erro -> do
              adicionarLog (criarLogFalha time Add erro)
              putStrLn erro
              return inv
        Nothing -> do
          putStrLn "Quantidade invalida!"
          return inv
    
    "2" -> do
      -- Opção 2: Remover item
      if Map.null inv
        then do
          putStrLn "Inventario vazio, nao tem o que remover."
          let logEntry = criarLogFalha time Remove "Inventario vazio"
          adicionarLog logEntry
          return inv
        else do
          putStr "ID do item: "
          hFlush stdout
          itemId <- getLine
          
          
          if not (Map.member itemId inv)
            then do
              let erro = "Erro: Item nao encontrado"
              adicionarLog (criarLogFalha time Remove erro)
              putStrLn erro
              return inv
            else do
              -- só pede a quantidade se o item existir (id existe- confirma primeiro)
              putStr "Quantidade a remover: "
              hFlush stdout
              qtdStr <- getLine
              
              case readMaybe qtdStr of
                Just qtd ->
                  case removeItem time itemId qtd inv of
                    Right (novoInv, logEntry) -> do
                      salvarInventario novoInv
                      adicionarLog logEntry
                      putStrLn "Item removido com sucesso!"
                      return novoInv
                    Left erro -> do
                      adicionarLog (criarLogFalha time Remove erro)
                      putStrLn erro
                      return inv
                Nothing -> do
                  putStrLn "Quantidade invalida!"
                  return inv
    
    "3" -> do
      -- Opção 3: Atualizar item
      if Map.null inv
        then do
          putStrLn "Inventario vazio nao ha o que atualizar."
          let logEntry = criarLogFalha time Update "Inventario vazio"
          adicionarLog logEntry
          return inv
        else do
          putStr "ID do item: "
          hFlush stdout
          itemId <- getLine
          
          
          if not (Map.member itemId inv)
            then do
              let erro = "Erro: Item nao encontrado"
              adicionarLog (criarLogFalha time Update erro)
              putStrLn erro
              return inv
            else do
              
              putStr "Nova quantidade: "
              hFlush stdout
              qtdStr <- getLine
              
              case readMaybe qtdStr of
                Just qtd ->
                  case updateItem time itemId qtd inv of
                    Right (novoInv, logEntry) -> do
                      salvarInventario novoInv
                      adicionarLog logEntry
                      putStrLn "Item atualizado com sucesso!"
                      return novoInv
                    Left erro -> do
                      adicionarLog (criarLogFalha time Update erro)
                      putStrLn erro
                      return inv
                Nothing -> do
                  putStrLn "Quantidade invalida!"
                  return inv
    
    "4" -> do
      listarInventario inv
      return inv
    
    "5" -> do
      gerarRelatorio
      return inv
    
    "0" -> do
      putStrLn "Encerrando..."
      return inv
    
    _ -> do
      putStrLn "Opcao invalida!"
      return inv


-- IDs são ordenados de forma crescente
listarInventario :: Inventario -> IO ()
listarInventario inv
  | Map.null inv = putStrLn "\nInventario vazio!\n"
  | otherwise = do
      putStrLn "\nINVENTARIO"
      mapM_ printItem (sortByID $ Map.toList inv)  -- ordena com sort
     
  where
    printItem (id, item) = 
      putStrLn $ id ++ " | " ++ nome item ++ 
                 " | Qtd: " ++ show (quantidade item) ++ 
                 " | Cat: " ++ categoria item
    
    -- ordena
    sortByID = sortBy compareIDs
    
    -- compara os ids, como IDS podem ser numericos (ex:1) ou textuasi (A1) é importante manter a ordenação em todos os casos
    -- por isso tem essa comparação
    compareIDs (id1, _) (id2, _) = 
      case (readMaybe id1 :: Maybe Int, readMaybe id2 :: Maybe Int) of
        (Just n1, Just n2) -> compare n1 n2  
        (Just _, Nothing)  -> LT              -- numero vem antes de texto
        (Nothing, Just _)  -> GT              -- Texto vem depois de número
        (Nothing, Nothing) -> compare id1 id2 
--relatorio

-- Filtrar logs de erro
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isErro
  where
    isErro entry = case status entry of
      Falha _ -> True
      _ -> False

-- Histórico por item
historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem itemId = filter (contemItem itemId)
  where
    contemItem id entry = id `elem` words (detalhes entry)

-- Item mais movimentado
itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado logs =
  case Map.toList frequencias of
    [] -> Nothing
    lista -> Just $ maximumBy (\(_, c1) (_, c2) -> compare c1 c2) lista
  where
    frequencias = foldr contarItem Map.empty logs
    contarItem entry acc = 
      case extrairID (detalhes entry) of
        Just itemId -> Map.insertWith (+) itemId 1 acc
        Nothing -> acc
    extrairID str = 
      let ws = words str
      in if length ws >= 2 then Just (ws !! 1) else Nothing
    maximumBy f (x:xs) = foldl (\a b -> if f a b == GT then a else b) x xs
    maximumBy _ [] = error "Lista vazia"

-- Gerar relatório
gerarRelatorio :: IO ()
gerarRelatorio = do
  logs <- carregarLogs
  
  putStrLn "\nRELATORIO DE AUDITORIA"
  putStrLn $ "Total de operacoes: " ++ show (length logs)
  
  let sucessos = length $ filter isSucesso logs
  putStrLn $ "Operacoes bem-sucedidas: " ++ show sucessos
  
  let erros = logsDeErro logs
  putStrLn $ "Operacoes com falha: " ++ show (length erros)
  
  when (not $ null erros) $ do
    putStrLn "\n--- DETALHES DOS ERROS ---"
    mapM_ printErro erros
  
  case itemMaisMovimentado logs of
    Just (itemId, count) -> 
      putStrLn $ "\nItem mais movimentado: " ++ itemId ++ " (" ++ show count ++ " operacoes)"
    Nothing -> 
      putStrLn "\nNenhum item movimentado."
  

  
  where
    isSucesso entry = case status entry of
      Sucesso -> True
      _ -> False
    printErro entry = do
      let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (timestamp entry)
      putStrLn $ "[" ++ timeStr ++ "] " ++ detalhes entry
    when False _ = return ()
    when True action = action

-- Loop principal
loop :: Inventario -> IO ()
loop inv = do
  mostrarMenu
  opcao <- getLine
  if opcao == "0"
    then putStrLn "Ate logo!"
    else do
      novoInv <- processarOpcao opcao inv
      loop novoInv

-- Main
main :: IO ()
main = do
  putStrLn "SISTEMA DE GERENCIAMENTO HASKELL"

  
  inv <- carregarInventario
  
  if Map.null inv
    then putStrLn "Inventario vazio. Iniciando novo inventario."
    else putStrLn $ "Inventario carregado: " ++ show (Map.size inv) ++ " itens."
  
  loop inv
  

