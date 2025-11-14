# Sistema de Gerenciamento de Inventário em Haskell

Este projeto implementa um **sistema completo de inventário**, com armazenamento em arquivo, operações CRUD, auditoria com logs detalhados e geração de relatórios.  
O sistema é totalmente interativo via terminal e segue as orientações conforme explicação da atividade

---

## Aluna
**Emily Pontes Fontana - 4u noite**

---

## Principais Funcionalidades

### CRUD completo
- **Adicionar itens**
- **Remover itens**
- **Atualizar quantidade**
- **Listar inventário ordenado**

###  Armazenamento em arquivos
- `Inventario.dat` → salva o estado do inventário  
- `Auditoria.log` → registra todas as operações

###  Auditoria e tratamento de erros
O sistema registra:
- Operações bem-sucedidas  
- Falhas (item inexistente, quantidade inválida, etc.)  
- Timestamp de cada operação  
- Ação realizada (Add, Remove, Update)

###  Relatórios automáticos
- Total de operações
- Sucessos e falhas
- Erros detalhados
- Item mais movimentado
- Histórico por item

---

** O item foi realizado e exucatado por meio do GDB Compiler na linguagem Haskell**

---

**Interação realizada por meio de um menu**
